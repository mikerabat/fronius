/* ==========================================================================
   Solar Monitor – gemeinsame Hilfsfunktionen (ohne jQuery)
   ========================================================================== */
'use strict';

const API = '/fronius/';

/** GET/POST gegen das Apache-Modul. Liefert immer das geparste JSON. */
async function api(path, { method = 'GET', body = null, params = null } = {}) {
  let url = API + path;
  if (params) {
    const q = new URLSearchParams();
    for (const [k, v] of Object.entries(params)) if (v !== undefined && v !== null) q.append(k, v);
    const qs = q.toString();
    if (qs) url += (url.includes('?') ? '&' : '?') + qs;
  }
  const opt = { method, credentials: 'include', cache: 'no-store', headers: {} };
  if (body !== null) {
    opt.headers['Content-Type'] = 'application/json; charset=utf-8';
    opt.body = typeof body === 'string' ? body : JSON.stringify(body);
  }
  const res = await fetch(url, opt);
  if (!res.ok) throw new Error('HTTP ' + res.status);
  const txt = await res.text();
  try { return JSON.parse(txt); }
  catch (e) { throw new Error('Ungültige Antwort vom Server'); }
}

/** Entfernt das Session-Cookie auf allen Pfaden (wie bisher sessionUtils.js). */
function eraseSessionCookie(name = 'SessionID') {
  const exp = '=; expires=Thu, 01 Jan 1970 00:00:01 GMT;';
  document.cookie = name + exp;
  let p = '';
  for (const part of location.pathname.split('/')) {
    p += (p.endsWith('/') ? '' : '/') + part;
    document.cookie = name + exp + ' path=' + p + ';';
  }
}

/* ---------- Formatierung ---------- */
const LOCALE = 'de-AT';
const nf = (d) => new Intl.NumberFormat(LOCALE, { minimumFractionDigits: d, maximumFractionDigits: d });
const NF0 = nf(0), NF1 = nf(1), NF2 = nf(2);

/** Leistung in W -> {v, u} (W bzw. kW) */
function fmtPower(w) {
  if (w === null || w === undefined || isNaN(w)) return { v: '–', u: 'W' };
  const a = Math.abs(w);
  if (a >= 1000) return { v: (a >= 10000 ? NF1 : NF2).format(w / 1000), u: 'kW' };
  return { v: NF0.format(w), u: 'W' };
}
const powerStr = (w) => { const p = fmtPower(w); return p.v + ' ' + p.u; };

/** Energie in Wh -> {v, u} (Wh / kWh / MWh) */
function fmtEnergy(wh) {
  if (wh === null || wh === undefined || isNaN(wh)) return { v: '–', u: 'kWh' };
  const a = Math.abs(wh);
  if (a >= 1e6) return { v: NF2.format(wh / 1e6), u: 'MWh' };
  if (a >= 1000) return { v: (a >= 100000 ? NF0 : NF1).format(wh / 1000), u: 'kWh' };
  return { v: NF0.format(wh), u: 'Wh' };
}
const energyStr = (wh) => { const e = fmtEnergy(wh); return e.v + ' ' + e.u; };
const pctStr = (x) => (x === null || x === undefined || isNaN(x)) ? '–' : NF0.format(x * 100) + ' %';

const MONTHS = ['Jänner','Februar','März','April','Mai','Juni','Juli','August','September','Oktober','November','Dezember'];
const MONTHS_SHORT = ['Jän','Feb','Mär','Apr','Mai','Jun','Jul','Aug','Sep','Okt','Nov','Dez'];
const WEEKDAYS = ['So','Mo','Di','Mi','Do','Fr','Sa'];

const pad2 = (n) => String(n).padStart(2, '0');

function cssVar(name) {
  return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
}

function escapeHtml(s) {
  return String(s).replace(/[&<>"']/g, c => ({ '&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;' }[c]));
}

/* ---------- Icons (inline SVG, currentColor) ---------- */
const ICONS = {
  sun: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"><circle cx="12" cy="12" r="4.2"/><path d="M12 2.5v2.2M12 19.3v2.2M4.6 4.6l1.6 1.6M17.8 17.8l1.6 1.6M2.5 12h2.2M19.3 12h2.2M4.6 19.4l1.6-1.6M17.8 6.2l1.6-1.6"/></svg>',
  panel: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" stroke-linecap="round"><path d="M5 9h14l2 8H3z"/><path d="M4 13h16M9.7 9l-1 8M14.3 9l1 8M12 17v3.5M8.5 20.5h7"/><path d="M8 5.2l1 1M12 3.5v1.4M16 5.2l-1 1"/></svg>',
  house: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" stroke-linecap="round"><path d="M3.5 11 12 4l8.5 7"/><path d="M5.5 9.5V20h13V9.5"/><path d="M10 20v-5.5h4V20"/></svg>',
  grid: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" stroke-linecap="round"><path d="M12 2.5 8 21.5M12 2.5l4 19M9.3 15h5.4M10 11.5h4M7 6.5h10M5 6.5v2M19 6.5v2M6.5 10h11"/></svg>',
  key: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"><circle cx="8" cy="15" r="4.5"/><path d="M11.2 11.8 20 3M16.5 6.5l2.5 2.5M14 9l2 2"/></svg>',
  logout: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"><path d="M15 4h3a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2h-3"/><path d="M10 17l-5-5 5-5M5 12h11"/></svg>',
  left: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="m15 18-6-6 6-6"/></svg>',
  right: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="m9 18 6-6-6-6"/></svg>',
  calendar: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"><rect x="3.5" y="5" width="17" height="15.5" rx="2.5"/><path d="M3.5 10h17M8 3v4M16 3v4"/></svg>',
  table: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"><rect x="3.5" y="4.5" width="17" height="15" rx="2"/><path d="M3.5 9.5h17M3.5 14.5h17M10 9.5v10"/></svg>',
  theme: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"><circle cx="12" cy="12" r="8.5"/><path d="M12 3.5v17a8.5 8.5 0 0 0 0-17z" fill="currentColor"/></svg>',
};

/** App-Logo (identisch mit favicon.svg / apple-touch-icon) */
const LOGO_SVG = '<svg viewBox="0 0 512 512" aria-hidden="true">' +
  '<defs><linearGradient id="lg-bg" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#21426e"/><stop offset="1" stop-color="#0e1c31"/></linearGradient>' +
  '<linearGradient id="lg-pn" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#3a7bc8"/><stop offset="1" stop-color="#22579a"/></linearGradient></defs>' +
  '<rect width="512" height="512" rx="114" fill="url(#lg-bg)"/>' +
  '<g stroke="#f7b500" stroke-width="22" stroke-linecap="round"><path d="M348 48v34M348 218v34M244 150h34M418 150h34M274 76l24 24M398 200l24 24M422 76l-24 24M298 200l-24 24"/></g>' +
  '<circle cx="348" cy="150" r="54" fill="#f7b500"/>' +
  '<path d="M138 262h236l58 170H80z" fill="url(#lg-pn)" stroke="#cfe2fb" stroke-width="12" stroke-linejoin="round"/>' +
  '<path d="M119 318.7h274M99.4 375.3h313.2M197 262l-19.5 170M256 262v170M315 262l19.5 170" stroke="#cfe2fb" stroke-width="8" opacity=".85"/>' +
  '<path d="M150 274l-10 30h44l7-30z" fill="#fff" opacity=".22"/></svg>';

/** Detaillierte Icons (64er Raster) für die Energiefluss-Grafik */
const ICONS_LG = {
  pv:
    '<svg viewBox="0 0 64 64" aria-hidden="true">' +
    '<g stroke="var(--c-pv)" stroke-width="2.6" stroke-linecap="round"><path d="M47 2.5v3.4M47 24.1v2.6M34.5 15h3.4M56.1 15h3.4M38.2 6.2l2.4 2.4M53.4 21.4l1.9 1.9M55.8 6.2l-2.4 2.4"/></g>' +
    '<circle cx="47" cy="15" r="6.6" fill="var(--c-pv)"/>' +
    '<path d="M10 28h36l8 20H2z" fill="var(--ico-panel)"/>' +
    '<path d="M7.3 34.7h41.4M4.7 41.3h46.6M19 28l-4 20M28 28v20M37 28l4 20" stroke="var(--ico-cell)" stroke-width="1.5" fill="none"/>' +
    '<path d="M12 29.6l-2.2 4h8l1.2-4z" fill="#fff" opacity=".28"/>' +
    '<path d="M10 28h36l8 20H2z" fill="none" stroke="var(--ico-frame)" stroke-width="2" stroke-linejoin="round"/>' +
    '<path d="M28 48v8.5M19 58h18" stroke="var(--ico-frame)" stroke-width="3" stroke-linecap="round" fill="none"/>' +
    '</svg>',
  house:
    '<svg viewBox="0 0 64 64" aria-hidden="true">' +
    '<rect x="42" y="11" width="6.5" height="14" rx="1.2" fill="var(--ico-frame)"/>' +
    '<path d="M12 29v25.5a2.5 2.5 0 0 0 2.5 2.5h35a2.5 2.5 0 0 0 2.5-2.5V29L32 12z" fill="var(--ico-wall)" stroke="var(--ico-frame)" stroke-width="2" stroke-linejoin="round"/>' +
    '<path d="M6 31.5 32 9.5l26 22" fill="none" stroke="var(--c-load)" stroke-width="5.5" stroke-linecap="round" stroke-linejoin="round"/>' +
    '<rect x="16" y="34" width="9" height="9" rx="1.4" fill="var(--ico-window)" stroke="var(--ico-frame)" stroke-width="1.6"/>' +
    '<rect x="39" y="34" width="9" height="9" rx="1.4" fill="var(--ico-window)" stroke="var(--ico-frame)" stroke-width="1.6"/>' +
    '<path d="M20.5 34v9M16 38.5h9M43.5 34v9M39 38.5h9" stroke="var(--ico-frame)" stroke-width="1.2"/>' +
    '<rect x="27.5" y="39" width="9" height="18" rx="1.5" fill="var(--c-load)"/>' +
    '<circle cx="34" cy="48.5" r="1.1" fill="#fff"/>' +
    '</svg>',
  grid:
    '<svg viewBox="0 0 64 64" aria-hidden="true">' +
    '<g fill="none" stroke="var(--text-3)" stroke-width="1.3"><path d="M1 27q7-3 14-5M49 22q7 2 14 5M1 39q9-3 18-5M45 34q9 2 18 5"/></g>' +
    '<g fill="none" stroke="var(--ico-frame)" stroke-width="2.4" stroke-linejoin="round" stroke-linecap="round">' +
    '<path d="M22 60 29 8l3-5 3 5 7 52"/>' +
    '<path d="M13.5 18h37M17.5 30h29M24.4 42h15.2M18 60h28"/>' +
    '<path d="M27.7 18l10.2 12-13.5 12L42 60M36.3 18 26.1 30l13.5 12L22 60" stroke-width="1.6"/>' +
    '</g>' +
    '<g stroke="var(--c-grid)" stroke-width="2.6" stroke-linecap="round"><path d="M15 18v4M49 18v4M19 30v4M45 30v4"/></g>' +
    '</svg>',
};

/* ---------- Theme (hell/dunkel/automatisch, pro Browser gemerkt) ---------- */
function applyStoredTheme() {
  try {
    const t = localStorage.getItem('solar.theme');
    if (t === 'light' || t === 'dark') document.documentElement.setAttribute('data-theme', t);
  } catch (e) { /* ignore */ }
}
function toggleTheme() {
  const root = document.documentElement;
  const isDark = root.getAttribute('data-theme')
    ? root.getAttribute('data-theme') === 'dark'
    : matchMedia('(prefers-color-scheme: dark)').matches;
  const next = isDark ? 'light' : 'dark';
  root.setAttribute('data-theme', next);
  try { localStorage.setItem('solar.theme', next); } catch (e) { /* ignore */ }
  document.dispatchEvent(new CustomEvent('themechange'));
}
applyStoredTheme();
