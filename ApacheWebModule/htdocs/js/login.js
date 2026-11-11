/* ==========================================================================
   Solar Monitor – Passkey-Anmeldung (WebAuthn, ohne Benutzername)

   Nutzt unverändert die Endpunkte des Apache-Moduls:
     /fronius/checksession, /fronius/settings,
     /fronius/assertstart, /fronius/assertverify,
     /fronius/enrollstart,  /fronius/enrollverify

   Ablauf "Mit Passkey anmelden":
     1. assertstart ohne uname  -> Server liefert Challenge mit leerer
        allowCredentials-Liste (discoverable credential / Passkey).
     2. Der Browser zeigt die Passkey-Auswahl, der Benutzer wählt.
     3. Aus dem userHandle des Passkeys wird der Benutzername gewonnen
        (neue Registrierungen speichern "u:<name>" als user.id). Für alte
        Passkeys wird die gemerkte Zuordnung (credential id -> name) aus dem
        Browser verwendet oder einmalig nachgefragt.
     4. assertverify bekommt den Benutzernamen im Feld userHandle – genau so
        sucht der Server den Benutzer (IsAlreadRegistered(userHandle)).
        Die Signaturprüfung erfolgt serverseitig gegen den gespeicherten
        Schlüssel; der Name ist nur ein Lookup-Hinweis.
   ========================================================================== */
'use strict';

const HANDLE_PREFIX = 'u:';
const MAP_KEY = 'solar.passkeys';

let settings = null;          // Antwort von /fronius/settings
let pendingAssertion = null;  // wartet auf Benutzername (alte Passkeys)

/* ---------- base64url ---------- */
function b64urlToBuf(s) {
  s = String(s).replace(/-/g, '+').replace(/_/g, '/');
  while (s.length % 4) s += '=';
  const bin = atob(s);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}
function bufToB64url(buf) {
  const bytes = new Uint8Array(buf);
  let bin = '';
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

/* ---------- Benutzername <-> userHandle ---------- */
const normalizeName = (n) => String(n || '').trim().toLowerCase().replace(/\s+/g, '');

function handleFromName(uname) {
  return new TextEncoder().encode(HANDLE_PREFIX + uname);
}
function nameFromHandle(buf) {
  if (!buf || buf.byteLength === 0) return null;
  try {
    const s = new TextDecoder('utf-8', { fatal: true }).decode(buf);
    return s.startsWith(HANDLE_PREFIX) ? s.slice(HANDLE_PREFIX.length) : null;
  } catch (e) { return null; }
}
function loadMap() {
  try { return JSON.parse(localStorage.getItem(MAP_KEY) || '{}') || {}; } catch (e) { return {}; }
}
function rememberCred(credId, uname) {
  try { const m = loadMap(); m[credId] = uname; localStorage.setItem(MAP_KEY, JSON.stringify(m)); } catch (e) { /* ignore */ }
}

/* ---------- UI ---------- */
const $ = (id) => document.getElementById(id);

function showMsg(text, kind = '') {
  const el = $('msg');
  if (!text) { el.classList.add('hidden'); return; }
  el.className = 'msg' + (kind ? ' ' + kind : '');
  el.textContent = text;
}

function setBusy(btn, busy, label) {
  if (!btn) return;
  btn.disabled = busy;
  const txt = btn.querySelector('.txt');
  const ico = btn.querySelector('.ico');
  if (busy) {
    btn.dataset.label = txt ? txt.textContent : '';
    if (ico) ico.innerHTML = '<span class="spinner"></span>';
    if (txt && label) txt.textContent = label;
  } else {
    if (ico) ico.innerHTML = ICONS[ico.dataset.icon] || '';
    if (txt && btn.dataset.label) txt.textContent = btn.dataset.label;
  }
}

function uvSetting() {
  // Der Server prüft das UV-Flag exakt gegen seine Einstellung
  const uv = settings && settings.publicKey && settings.publicKey.authenticatorSelection
    ? settings.publicKey.authenticatorSelection.userVerification : 'discouraged';
  return uv === 'required' ? 'required' : 'discouraged';
}
function rpId() {
  return (settings && settings.publicKey && settings.publicKey.rp && settings.publicKey.rp.id) || location.hostname;
}

/** Übersetzt Server- und Browserfehler in verständliche Meldungen */
function explainServerError(r) {
  const code = r && r.error;
  const msg = (r && r.msg) || '';
  if (code === 2) return 'Dieser Passkey ist dem Server nicht (mehr) bekannt oder nicht der primäre Passkey des Benutzers.';
  if (code === 4) return 'Der Server erwartet eine andere Benutzerüberprüfung (PIN/Biometrie) als dein Passkey liefert. ' +
                         'Bitte einen Sicherheitsschlüssel ohne PIN verwenden oder serverseitig UserVerification aktivieren.';
  if (code === 1 && /Challenge/i.test(msg)) return 'Die Anmeldung hat zu lange gedauert. Bitte erneut versuchen.';
  if (code === -1) return 'Die Datenbank ist nicht erreichbar.';
  if (code === -254) return 'Neue Passkeys können nur aus dem Heimnetz eingerichtet werden.';
  return msg || 'Unbekannter Fehler.';
}
function explainDomError(err) {
  if (!err) return 'Unbekannter Fehler.';
  if (err.name === 'NotAllowedError') return 'Abgebrochen oder Zeitüberschreitung.';
  if (err.name === 'InvalidStateError') return 'Für dieses Konto existiert auf diesem Authenticator bereits ein Passkey.';
  if (err.name === 'SecurityError') return 'Sicherheitsfehler – die Seite muss über HTTPS mit der richtigen Domain geöffnet werden.';
  return err.message || String(err);
}

/* ---------- Anmeldung ---------- */
async function startAssertion(uname) {
  const opts = await api('assertstart', { params: uname ? { uname } : null });
  if (opts.error !== undefined) throw new Error(explainServerError(opts));
  const pk = opts.publicKey;
  const req = {
    challenge: b64urlToBuf(pk.challenge),
    timeout: pk.timeout || 60000,
    rpId: rpId(),
    userVerification: uvSetting(),
    allowCredentials: (pk.allowCredentials || []).map(c => ({ type: 'public-key', id: b64urlToBuf(c.id) })),
  };
  return navigator.credentials.get({ publicKey: req });
}

async function verifyAssertion(cred, unameHint) {
  const r = cred.response;
  const payload = {
    id: cred.id,
    rawId: bufToB64url(cred.rawId),
    type: cred.type,
    response: {
      authenticatorData: bufToB64url(r.authenticatorData),
      clientDataJSON:    bufToB64url(r.clientDataJSON),
      signature:         bufToB64url(r.signature),
      // Der Server sucht den Benutzer über dieses Feld (Benutzername)
      userHandle:        unameHint || (r.userHandle ? bufToB64url(r.userHandle) : ''),
    },
  };
  const res = await api('assertverify', { method: 'POST', body: payload });
  if (res.verified) {
    if (unameHint) rememberCred(cred.id, unameHint);
    showMsg('Angemeldet – einen Moment …', 'ok');
    location.replace('/index.html');
    return true;
  }
  throw new Error(explainServerError(res));
}

async function loginWithPasskey() {
  showMsg('');
  const btn = $('btn-login');
  setBusy(btn, true, 'Warte auf Passkey …');
  try {
    const cred = await startAssertion('');
    const uname = nameFromHandle(cred.response.userHandle) || loadMap()[cred.id];
    if (!uname) {
      // alter Passkey: Benutzernamen einmalig erfragen, Assertion bleibt gültig
      pendingAssertion = cred;
      $('legacy-user').classList.remove('hidden');
      btn.classList.add('hidden');
      $('legacy-name').focus();
      return;
    }
    await verifyAssertion(cred, uname);
  } catch (err) {
    showMsg(err instanceof DOMException ? explainDomError(err) : err.message, 'error');
  } finally {
    setBusy(btn, false);
  }
}

async function finishLegacy(ev) {
  ev.preventDefault();
  const uname = normalizeName($('legacy-name').value);
  if (!uname || !pendingAssertion) return;
  showMsg('');
  try {
    await verifyAssertion(pendingAssertion, uname);
  } catch (err) {
    showMsg(err.message, 'error');
    pendingAssertion = null;
    $('legacy-user').classList.add('hidden');
    $('btn-login').classList.remove('hidden');
  }
}

/** Klassischer Weg: Benutzername -> Server schickt die erlaubte Credential-ID */
async function loginWithUsername(ev) {
  ev.preventDefault();
  const uname = normalizeName($('uname-login').value);
  if (!uname) { showMsg('Bitte Benutzernamen eingeben.', 'error'); return; }
  showMsg('Warte auf Passkey …');
  try {
    const exists = await api('userexists', { params: { uname } });
    if (exists.result !== 2) throw new Error(exists.msg || 'Benutzer nicht gefunden.');
    const cred = await startAssertion(uname);
    await verifyAssertion(cred, uname);
  } catch (err) {
    showMsg(err instanceof DOMException ? explainDomError(err) : err.message, 'error');
  }
}

/* ---------- Registrierung ---------- */
async function enroll(ev) {
  ev.preventDefault();
  const display = $('enroll-name').value.trim();
  const uname = normalizeName(display);
  if (!uname) { showMsg('Bitte einen Namen eingeben.', 'error'); return; }
  showMsg('Passkey wird erstellt …');
  try {
    const r = await api('enrollstart', { params: { uname, realname: display } });
    if (r.error !== undefined) throw new Error(explainServerError(r));
    if (r.result === 1) throw new Error('Kein Name angegeben.');
    if (r.result === 2) throw new Error('Dieser Benutzer hat bereits einen Passkey.');
    const pk = r.publicKey;

    const cred = await navigator.credentials.create({
      publicKey: {
        rp: { id: pk.rp.id, name: pk.rp.name || 'Solar Monitor' },
        // user.id enthält den Namen -> späteres Anmelden ganz ohne Eingabe
        user: { id: handleFromName(uname), name: pk.user.name || uname, displayName: display || uname },
        challenge: b64urlToBuf(pk.challenge),
        pubKeyCredParams: pk.pubKeyCredParams || [{ type: 'public-key', alg: -7 }, { type: 'public-key', alg: -257 }],
        timeout: 120000,
        attestation: 'none',          // Server akzeptiert "none" (MinAllowedAttestation)
        authenticatorSelection: {     // kein authenticatorAttachment -> Gerät, Handy oder Schlüssel
          residentKey: 'required',
          requireResidentKey: true,
          userVerification: uvSetting(),
        },
      },
    });

    const res = await api('enrollverify', {
      method: 'POST',
      body: {
        id: cred.id,
        rawId: bufToB64url(cred.rawId),
        type: cred.type,
        response: {
          attestationObject: bufToB64url(cred.response.attestationObject),
          clientDataJSON:    bufToB64url(cred.response.clientDataJSON),
        },
      },
    });
    if (res.error !== undefined) throw new Error(explainServerError(res));
    if (!res.verified) throw new Error('Der Passkey konnte nicht bestätigt werden.');
    rememberCred(cred.id, uname);
    showMsg('Passkey eingerichtet – du wirst angemeldet …', 'ok');
    location.replace('/index.html');
  } catch (err) {
    showMsg(err instanceof DOMException ? explainDomError(err) : err.message, 'error');
  }
}

/* ---------- Start ---------- */
async function init() {
  $('logo').innerHTML = LOGO_SVG;
  document.querySelectorAll('[data-icon]').forEach(el => { el.innerHTML = ICONS[el.dataset.icon] || ''; });

  $('btn-login').addEventListener('click', loginWithPasskey);
  $('legacy-user').addEventListener('submit', finishLegacy);
  $('form-uname-login').addEventListener('submit', loginWithUsername);
  $('form-enroll').addEventListener('submit', enroll);

  if (!window.PublicKeyCredential) {
    $('btn-login').disabled = true;
    showMsg(location.protocol === 'http:' && !/^(localhost|127\.0\.0\.1)$/.test(location.hostname)
      ? 'Passkeys funktionieren nur über HTTPS.'
      : 'Dieser Browser unterstützt keine Passkeys.', 'error');
    return;
  }

  try {
    const s = await api('checksession');
    if (s.session === 1) { location.replace('/index.html'); return; }
    eraseSessionCookie('SessionID');
  } catch (e) { /* bleibt auf der Login-Seite */ }

  try {
    settings = await api('settings');
    if (settings.error !== undefined) throw new Error(explainServerError(settings));
    if (settings.AllowEnroll) $('enroll').classList.remove('hidden');
    if (settings.CanLogin === false) {
      // noch kein Benutzer angelegt -> Einrichtung zeigen
      $('btn-login').classList.add('hidden');
      $('form-uname-login').classList.add('hidden');
      $('other').open = true;
      $('other').querySelector('summary').classList.add('hidden');
      showMsg(settings.AllowEnroll
        ? 'Es ist noch kein Passkey eingerichtet. Lege jetzt den ersten an.'
        : 'Es ist noch kein Passkey eingerichtet. Bitte die Seite aus dem Heimnetz öffnen.');
    }
  } catch (e) {
    showMsg('Server nicht erreichbar: ' + e.message, 'error');
  }
}

document.addEventListener('DOMContentLoaded', init);
