/* ==========================================================================
   Solar Monitor – Dashboard
   Datenquellen (unverändertes Apache-Modul):
     /fronius/checksession, /fronius/logout
     /fronius/cursamp                      aktueller Messwert (PPV, PL, PG …)
     /fronius/daysamples?date=dd.mm.yyyy   Tagesverlauf (Leistung in W)
     /fronius/bars?barType=0..3            Energie je Tag/Monat/Jahr/gesamt (Wh)
   Diagramme: Apache ECharts (lokal unter libs/echarts.min.js)
   ========================================================================== */
'use strict';

const LIVE_INTERVAL = 2000;       // ms
const FIRST_YEAR_GUESS = 2000;    // frühestes Jahr für die Gesamtabfrage
const REFRESH_INTERVAL = 5 * 60e3; // ms – Verlauf für "heute" neu laden
const $ = (id) => document.getElementById(id);

const state = {
  view: 'day',
  date: startOfDay(new Date()),   // Bezugsdatum für alle Ansichten
  firstYear: null,
  showTable: false,
};
const cache = new Map();          // key -> Promise
let chart = null;
let lastRender = null;            // { kind, payload } für Theme-Wechsel

/* ---------- Datum ---------- */
function startOfDay(d) { return new Date(d.getFullYear(), d.getMonth(), d.getDate()); }
function today() { return startOfDay(new Date()); }
function sameDay(a, b) { return a.getFullYear() === b.getFullYear() && a.getMonth() === b.getMonth() && a.getDate() === b.getDate(); }
function daysInMonth(y, m) { return new Date(y, m, 0).getDate(); }   // m: 1..12
function fmtDate(d) { return pad2(d.getDate()) + '.' + pad2(d.getMonth() + 1) + '.' + d.getFullYear(); }
function isoDate(d) { return d.getFullYear() + '-' + pad2(d.getMonth() + 1) + '-' + pad2(d.getDate()); }
function longDate(d) { return WEEKDAYS[d.getDay()] + ', ' + d.getDate() + '. ' + MONTHS[d.getMonth()] + ' ' + d.getFullYear(); }

/* ---------- API mit Session-Handling ---------- */
function toLogin() { eraseSessionCookie('SessionID'); location.replace('/login.html'); }

async function fetchData(path, params) {
  const r = await api(path, { params });
  if (r && r.error === -2) { toLogin(); throw new Error('Nicht angemeldet'); }
  return r;
}
function cached(key, loader, force = false) {
  if (force || !cache.has(key)) {
    const p = loader().catch(e => { cache.delete(key); throw e; });
    cache.set(key, p);
  }
  return cache.get(key);
}

/** Bar-Eintrag normalisieren (Wh) */
function norm(e) {
  const prod = Math.max(0, +e.enprod || 0);
  const cons = Math.max(0, +e.enconsume || 0);
  const imp  = Math.max(0, +e.engrid || 0);
  const exp  = Math.max(0, +e.entogrid || 0);
  return {
    prod, cons, imp, exp,
    selfUse: Math.max(0, prod - exp),   // Produktion, die selbst genutzt wurde
    direct:  Math.max(0, cons - imp),   // Verbrauch, der aus PV gedeckt wurde
    sdi: e.sdi,
  };
}
function sumEntries(list) {
  const s = { prod: 0, cons: 0, imp: 0, exp: 0 };
  let n = 0;
  for (const e of list) if (e) { s.prod += e.prod; s.cons += e.cons; s.imp += e.imp; s.exp += e.exp; n++; }
  if (!n) return null;
  s.selfUse = Math.max(0, s.prod - s.exp);
  s.direct = Math.max(0, s.cons - s.imp);
  return s;
}
const rates = (s) => ({
  selfRate: s && s.prod > 0 ? Math.min(1, s.selfUse / s.prod) : null,
  autarky:  s && s.cons > 0 ? Math.min(1, s.direct / s.cons) : null,
});

/* Loader */
function loadMonth(y, m, force) {
  return cached(`m${y}-${m}`, async () => {
    const r = await fetchData('bars', { barType: 0, fromYear: y, fromMon: m, toYear: y, toMon: m });
    const days = new Array(daysInMonth(y, m)).fill(null);
    for (const e of (r.data || [])) {
      if (e.sdi && e.sdi.year === y && e.sdi.month === m) days[e.sdi.day - 1] = norm(e);
    }
    return days;
  }, force);
}
function loadYear(y, force) {
  return cached(`y${y}`, async () => {
    const r = await fetchData('bars', { barType: 1, fromYear: y, fromMon: 1, toYear: y, toMon: 12 });
    const months = new Array(12).fill(null);
    for (const e of (r.data || [])) if (e.sdi && e.sdi.month >= 1 && e.sdi.month <= 12) months[e.sdi.month - 1] = norm(e);
    return months;
  }, force);
}
function loadYears(force) {
  return cached('years', async () => {
    // Startjahr explizit setzen: ohne fromYear nimmt der Server das erste Jahr
    // aus der Tabelle SAMPLES (die nur die letzten Tage enthält) -> nur 1 Jahr.
    const r = await fetchData('bars', { barType: 2, fromYear: FIRST_YEAR_GUESS, fromMon: 1, toYear: today().getFullYear(), toMon: 12 });
    const list = (r.data || []).map(norm).filter(e => e.sdi && (e.prod || e.cons));
    list.sort((a, b) => a.sdi.year - b.sdi.year);
    return list;
  }, force);
}
function loadTotal(force) {
  return cached('total', async () => {
    const r = await fetchData('bars', { barType: 3 });
    if (r.error !== undefined || !r.data || !r.data.length) return null;
    return { total: norm(r.data[0]), from: r.fromDi };
  }, force);
}
function loadDaySamples(d, force) {
  return cached('d' + isoDate(d), async () => {
    const r = await fetchData('daysamples', { date: fmtDate(d) });
    return (r.data || []).map(s => {
      const pv = Math.max(0, +s.PPV || 0);
      const load = Math.abs(+s.PL || 0);
      const grid = +s.PG || 0;
      return { t: +s.t, pv, load, grid, self: Math.max(0, Math.min(pv, load)) };
    }).sort((a, b) => a.t - b.t);
  }, force);
}

/** Energie aus Leistungs-Samples integrieren (Näherung, Wh) – je Stunde */
function integrateHours(samples) {
  const hours = Array.from({ length: 24 }, () => ({ prod: 0, cons: 0, imp: 0, exp: 0 }));
  for (let i = 0; i < samples.length - 1; i++) {
    const a = samples[i], b = samples[i + 1];
    const dt = Math.min(900, Math.max(0, b.t - a.t)) / 3600;  // Lücken > 15 min nicht überbrücken
    if (!dt) continue;
    const h = hours[Math.min(23, Math.floor(a.t / 3600))];
    h.prod += (a.pv + b.pv) / 2 * dt;
    h.cons += (a.load + b.load) / 2 * dt;
    h.imp  += (Math.max(0, a.grid) + Math.max(0, b.grid)) / 2 * dt;
    h.exp  += (Math.max(0, -a.grid) + Math.max(0, -b.grid)) / 2 * dt;
  }
  return hours.map(h => ({ ...h, selfUse: Math.max(0, h.prod - h.exp), direct: Math.max(0, h.cons - h.imp) }));
}

/* ==========================================================================
   Live-Ansicht
   ========================================================================== */
const CIRC = 2 * Math.PI * 27;
function setRing(id, frac) {
  const tile = $(id);
  const fg = tile.querySelector('.fg');
  const txt = tile.querySelector('text');
  fg.style.strokeDasharray = CIRC;
  fg.style.strokeDashoffset = frac === null ? CIRC : CIRC * (1 - Math.max(0, Math.min(1, frac)));
  txt.textContent = frac === null ? '–' : NF0.format(frac * 100) + '%';
}

function setStream(id, ringId, watts, d) {
  const el = $(id);
  const on = watts > 5;
  if (d) el.setAttribute('d', d);
  el.style.opacity = on ? 1 : 0;
  el.style.strokeWidth = 3 + Math.min(5, watts / 1500);
  el.style.animationDuration = (on ? Math.max(0.35, 1.4 - watts / 6000) : 1) + 's';
  $(ringId).style.opacity = on ? 1 : 0.15;
}

function setVal(id, w) { $(id).textContent = powerStr(w); }

/* ---------- Energiefluss-Grafik (SVG, zwei Layouts) ---------- */
const FLOW_LAYOUTS = {
  // Desktop / Tablet quer
  wide: { w: 640, h: 340, r: 50, hub: [320, 234], pv: [320, 70], grid: [104, 234], load: [536, 234],
          fv: 24, fl: 14, pvText: 'side', textGap: 34 },
  // Handy hochkant
  compact: { w: 360, h: 316, r: 42, hub: [180, 206], pv: [180, 58], grid: [60, 206], load: [300, 206],
             fv: 23, fl: 14, pvText: 'side', textGap: 30 },
};
let flowLayout = null, gridDir = 'in', lastLive = null;

function buildFlow(kind) {
  const L = FLOW_LAYOUTS[kind], svg = $('flow'), r = L.r;
  const [hx, hy] = L.hub;
  L.dPv = `M${L.pv[0]} ${L.pv[1] + r} V${hy}`;
  L.dGridIn = `M${L.grid[0] + r} ${hy} H${hx}`;
  L.dGridOut = `M${hx} ${hy} H${L.grid[0] + r}`;
  L.dLoad = `M${hx} ${hy} H${L.load[0] - r}`;
  const isz = Math.round(r * 1.28);
  const node = (id, [cx, cy], icon, color) =>
    `<g transform="translate(${cx} ${cy})"><circle class="node-bg" r="${r}"/>` +
    `<circle id="ring-${id}" class="node-ring" r="${r}" stroke="${color}"/>` +
    `<g transform="translate(${-isz / 2} ${-isz / 2 - 1})">${ICONS_LG[icon].replace('<svg ', `<svg width="${isz}" height="${isz}" `)}</g></g>`;
  const below = ([cx, cy], lblId, lbl, valId) =>
    `<text x="${cx}" y="${cy + r + L.textGap - 8}" class="lbl" text-anchor="middle" font-size="${L.fl}"${lblId ? ` id="${lblId}"` : ''}>${lbl}</text>` +
    `<text x="${cx}" y="${cy + r + L.textGap + L.fv - 4}" class="val" text-anchor="middle" font-size="${L.fv}" id="${valId}">–</text>`;
  const px = L.pv[0] + r + 14, py = L.pv[1];
  svg.setAttribute('viewBox', `0 0 ${L.w} ${L.h}`);
  svg.innerHTML =
    `<path class="track" d="${L.dPv}"/><path class="track" d="${L.dGridIn}"/><path class="track" d="${L.dLoad}"/>` +
    `<path id="s-pv" class="stream" d="${L.dPv}" stroke="var(--c-pv)"/>` +
    `<path id="s-grid" class="stream" d="${L.dGridIn}" stroke="var(--c-grid)"/>` +
    `<path id="s-load" class="stream" d="${L.dLoad}" stroke="var(--c-load)"/>` +
    `<circle class="hub" cx="${hx}" cy="${hy}" r="${Math.round(r / 5.5)}"/>` +
    node('pv', L.pv, 'pv', 'var(--c-pv)') + node('grid', L.grid, 'grid', 'var(--c-grid)') + node('load', L.load, 'house', 'var(--c-load)') +
    `<text x="${px}" y="${py - 6}" class="lbl" font-size="${L.fl}">Solar</text>` +
    `<text x="${px}" y="${py + L.fv - 2}" class="val" font-size="${L.fv}" id="v-pv">–</text>` +
    below(L.grid, 'l-grid', 'Netz', 'v-grid') + below(L.load, null, 'Verbrauch', 'v-load');
  flowLayout = { kind, ...L };
  if (lastLive) showLive(lastLive);
}

function watchFlowSize() {
  const pick = () => {
    const w = $('flow').parentElement.clientWidth;
    const kind = w < 520 ? 'compact' : 'wide';
    if (!flowLayout || flowLayout.kind !== kind) buildFlow(kind);
  };
  pick();
  new ResizeObserver(pick).observe($('flow').parentElement);
}

let liveTimer = null, liveFails = 0;
async function pollLive() {
  clearTimeout(liveTimer);
  try {
    const r = await fetchData('cursamp');
    if (r.error !== undefined) throw new Error(r.msg || 'Keine Daten');
    liveFails = 0;
    lastLive = r;
    showLive(r);
  } catch (e) {
    liveFails++;
    $('live-dot').className = 'live-dot off';
    $('live-time').textContent = 'Keine Verbindung' + (e.message ? ' – ' + e.message : '');
  }
  if (!document.hidden) liveTimer = setTimeout(pollLive, liveFails > 5 ? 10000 : LIVE_INTERVAL);
}

function showLive(r) {
  {
    const pv = Math.max(0, +r.PPV || 0);
    const load = Math.abs(+r.PL || 0);
    const grid = +r.PG || 0;
    const imp = Math.max(0, grid), exp = Math.max(0, -grid);
    const self = Math.max(0, pv - exp);

    setVal('v-pv', pv);
    setVal('v-load', load);
    setVal('v-grid', Math.abs(grid));
    $('l-grid').textContent = grid > 5 ? 'Netzbezug' : grid < -5 ? 'Einspeisung' : 'Netz';

    setStream('s-pv', 'ring-pv', pv);
    setStream('s-load', 'ring-load', load);
    const gEl = $('s-grid');
    gEl.setAttribute('stroke', grid < 0 ? 'var(--c-pv)' : 'var(--c-grid)');
    $('ring-grid').setAttribute('stroke', grid < 0 ? 'var(--c-pv)' : 'var(--c-grid)');
    setStream('s-grid', 'ring-grid', Math.abs(grid), grid < 0 ? flowLayout.dGridOut : flowLayout.dGridIn);

    setRing('rt-autarky', load > 0 ? Math.min(1, (load - imp) / load) : null);
    setRing('rt-selfcons', pv > 5 ? Math.min(1, self / pv) : null);
    $('k-self').textContent = powerStr(Math.min(self, load));
    $('k-export').textContent = powerStr(exp);
    $('k-import').textContent = powerStr(imp);

    // Zeitstempel "dd.mm.yyyy hh:nn:ss"
    const m = /(\d+)\.(\d+)\.(\d+)\s+(\d+):(\d+):(\d+)/.exec(r.t || '');
    let stale = false, label = r.t || '';
    if (m) {
      const ts = new Date(+m[3], +m[2] - 1, +m[1], +m[4], +m[5], +m[6]);
      stale = Date.now() - ts.getTime() > 120e3;
      label = (stale ? 'veraltet · ' : 'Live · ') + (sameDay(ts, new Date()) ? '' : fmtDate(ts) + ' ') + m[4].padStart(2, '0') + ':' + m[5] + ':' + m[6];
    }
    $('live-dot').className = 'live-dot ' + (stale ? 'off' : 'on');
    $('live-time').textContent = label;
  }
}

async function updateTodaySummary(force) {
  try {
    const t = today();
    const days = await loadMonth(t.getFullYear(), t.getMonth() + 1, force);
    const e = days[t.getDate() - 1];
    $('t-prod').textContent = e ? energyStr(e.prod) : '–';
    $('t-cons').textContent = e ? energyStr(e.cons) : '–';
  } catch (e) { /* ignore */ }
}

/* ==========================================================================
   KPI-Kacheln
   ========================================================================== */
function kpiTile(title, colorVar, wh, detail) {
  const e = fmtEnergy(wh);
  return `<div class="kpi"><div class="t">${colorVar ? `<span class="swatch" style="background:var(${colorVar})"></span>` : ''}${title}</div>
    <div class="v">${e.v}<small>${e.u}</small></div><div class="d">${detail || '&nbsp;'}</div></div>`;
}
function kpiRate(title, colorVar, frac, detail) {
  const w = frac === null ? 0 : Math.round(frac * 100);
  return `<div class="kpi"><div class="t">${title}</div>
    <div class="v">${frac === null ? '–' : NF0.format(frac * 100)}<small>%</small></div>
    <div class="kpi-bar"><i style="width:${w}%;background:var(${colorVar})"></i></div>
    <div class="d">${detail || '&nbsp;'}</div></div>`;
}
function renderKpis(s, note) {
  const el = $('kpis');
  if (!s) {
    el.innerHTML = kpiTile('Produktion', '--c-pv', null) + kpiTile('Verbrauch', '--c-load', null) +
      kpiTile('Netzbezug', '--c-grid', null) + kpiTile('Einspeisung', '--c-pv', null) +
      kpiRate('Eigenverbrauch', '--c-pv', null) + kpiRate('Autarkie', '--c-self', null);
    return;
  }
  const r = rates(s);
  const pc = (a, b) => b > 0 ? NF0.format(a / b * 100) + ' %' : '–';
  el.innerHTML =
    kpiTile('Produktion', '--c-pv', s.prod, `davon ${energyStr(s.selfUse)} selbst genutzt`) +
    kpiTile('Verbrauch', '--c-load', s.cons, `davon ${energyStr(s.direct)} aus PV`) +
    kpiTile('Netzbezug', '--c-grid', s.imp, `${pc(s.imp, s.cons)} des Verbrauchs`) +
    kpiTile('Einspeisung', '--c-pv', s.exp, `${pc(s.exp, s.prod)} der Produktion`) +
    kpiRate('Eigenverbrauch', '--c-pv', r.selfRate, 'Anteil PV selbst genutzt') +
    kpiRate('Autarkie', '--c-self', r.autarky, note || 'Verbrauch aus eigener PV');
}

/* ==========================================================================
   Diagramme
   ========================================================================== */
function mixHex(a, b, t) {
  const h = (x) => [1, 3, 5].map(i => parseInt(x.slice(i, i + 2), 16));
  const A = h(a), B = h(b);
  return '#' + A.map((v, i) => Math.round(v + (B[i] - v) * t).toString(16).padStart(2, '0')).join('');
}
function isDarkSurface() {
  const x = cssVar('--surface');
  return parseInt(x.slice(1, 3), 16) < 100;
}
function colors() {
  return {
    pv: cssVar('--c-pv'), self: cssVar('--c-self'), grid: cssVar('--c-grid'), load: cssVar('--c-load'),
    text: cssVar('--text'), text2: cssVar('--text-2'), text3: cssVar('--text-3'),
    border: cssVar('--border'), surface: cssVar('--surface'), surface2: cssVar('--surface-2'),
    // helle Variante für "selbst genutzt": im Dunkelmodus Richtung Text, sonst Richtung Fläche aufhellen
    pvSoft: isDarkSurface() ? mixHex(cssVar('--c-pv'), cssVar('--text'), .45) : mixHex(cssVar('--c-pv'), cssVar('--surface'), .55),
  };
}
function baseOption(c) {
  return {
    animationDuration: 400,
    textStyle: { fontFamily: getComputedStyle(document.body).fontFamily, color: c.text2 },
    grid: { left: 8, right: 24, top: 28, bottom: $('chart').clientWidth < 520 ? 62 : 36, containLabel: true },
    legend: {
      bottom: 0, left: 0, icon: 'roundRect', itemWidth: 12, itemHeight: 12, itemGap: 18,
      textStyle: { color: c.text2, fontSize: 13 },
    },
    tooltip: {
      backgroundColor: c.surface, borderColor: c.border, borderWidth: 1, padding: [10, 12],
      textStyle: { color: c.text, fontSize: 13 },
      extraCssText: 'border-radius:10px;box-shadow:0 6px 24px rgba(0,0,0,.18);',
      confine: true, triggerOn: 'mousemove|click',
    },
  };
}
const sw = (col) => `<span class="swatch" style="background:${col}"></span>`;
const ttRow = (col, label, val, pct, sub) =>
  `<div class="row${sub ? ' sub' : ''}">${col ? sw(col) : ''}${label}<b>${val}${pct ? `<span class="pct">${pct}</span>` : ''}</b></div>`;

/** Gestapelte Doppelbalken: links Verbrauch (PV direkt + Netzbezug), rechts Produktion (PV direkt + Einspeisung) */
function renderBars(labels, entries, titles) {
  const c = colors();
  const pc = (a, b) => b > 0 ? NF0.format(a / b * 100) + ' %' : '';
  const R = [4, 4, 0, 0];
  // Wert in kWh; abgerundete Ecke immer auf dem obersten sichtbaren Segment
  const seg = (f, isTop, other) => entries.map(e => {
    if (!e) return null;
    const v = +(f(e) / 1000).toFixed(2);
    const round = isTop ? v > 0 : other(e) <= 0;
    return { value: v, itemStyle: { borderRadius: round ? R : 0 } };
  });
  const base = { type: 'bar', itemStyle: { borderColor: c.surface, borderWidth: 1 } };
  const st = (color) => ({ ...base.itemStyle, color });

  const opt = baseOption(c);
  Object.assign(opt, {
    legend: { ...opt.legend, data: ['Verbrauch aus PV', 'Netzbezug', 'Einspeisung', 'Produktion selbst genutzt'] },
    tooltip: {
      ...opt.tooltip, trigger: 'axis', axisPointer: { type: 'shadow', shadowStyle: { color: c.text3, opacity: .12 } },
      formatter: (ps) => {
        const i = ps[0].dataIndex, e = entries[i];
        if (!e) return `<div class="tt"><h4>${titles[i]}</h4>Keine Daten</div>`;
        const r = rates(e);
        return `<div class="tt"><h4>${titles[i]}</h4>` +
          ttRow(c.load, '<b style="margin:0;padding:0">Verbrauch</b>', energyStr(e.cons)) +
          ttRow(c.self, 'aus PV', energyStr(e.direct), pc(e.direct, e.cons), true) +
          ttRow(c.grid, 'aus dem Netz', energyStr(e.imp), pc(e.imp, e.cons), true) +
          '<div class="sep"></div>' +
          ttRow(c.pv, '<b style="margin:0;padding:0">Produktion</b>', energyStr(e.prod)) +
          ttRow(c.pv, 'eingespeist', energyStr(e.exp), pc(e.exp, e.prod), true) +
          ttRow(c.pvSoft, 'selbst genutzt', energyStr(e.selfUse), pc(e.selfUse, e.prod), true) +
          '<div class="sep"></div>' +
          ttRow('', 'Autarkie', pctStr(r.autarky)) + ttRow('', 'Eigenverbrauch', pctStr(r.selfRate)) +
          '</div>';
      },
    },
    xAxis: {
      type: 'category', data: labels,
      axisLine: { lineStyle: { color: c.border } }, axisTick: { show: false },
      axisLabel: { color: c.text3, hideOverlap: true },
    },
    yAxis: {
      type: 'value', name: 'kWh', nameTextStyle: { color: c.text3, align: 'right' },
      splitLine: { lineStyle: { color: c.border, opacity: .7 } },
      axisLabel: { color: c.text3, formatter: (v) => NF0.format(v) },
    },
    series: [
      // linker Balken: Verbrauch = aus PV + Netzbezug
      { ...base, name: 'Verbrauch aus PV', stack: 'cons', data: seg(e => e.direct, false, e => e.imp), itemStyle: st(c.self),
        barGap: '12%', barCategoryGap: '28%' },
      { ...base, name: 'Netzbezug', stack: 'cons', data: seg(e => e.imp, true), itemStyle: st(c.grid) },
      // rechter Balken: Produktion = Einspeisung (unten) + selbst genutzt (oben, hell)
      { ...base, name: 'Einspeisung', stack: 'prod', data: seg(e => e.exp, false, e => e.selfUse), itemStyle: st(c.pv) },
      { ...base, name: 'Produktion selbst genutzt', stack: 'prod', data: seg(e => e.selfUse, true), itemStyle: st(c.pvSoft) },
    ],
  });
  if (!entries.some(Boolean)) opt.title = emptyTitle(c);
  setChart(opt);
  $('chart-note').textContent = 'Je Zeitraum zwei Balken: links Verbrauch (aus PV + Netzbezug), rechts Produktion (Einspeisung + selbst genutzt).';
}

function emptyTitle(c) {
  return { text: 'Keine Daten für diesen Zeitraum', left: 'center', top: 'middle', textStyle: { color: c.text3, fontSize: 14, fontWeight: 500 } };
}

const hhmm = (s) => pad2(Math.floor(s / 3600)) + ':' + pad2(Math.floor((s % 3600) / 60));

function renderDayChart(samples, d) {
  const c = colors();
  const opt = baseOption(c);
  const pts = (f) => samples.map(s => [s.t, f(s)]);
  Object.assign(opt, {
    legend: { ...opt.legend, data: ['Produktion', 'PV direkt genutzt', 'Verbrauch', 'Netz'] },
    tooltip: {
      ...opt.tooltip, trigger: 'axis',
      axisPointer: { type: 'line', lineStyle: { color: c.text3, width: 1 } },
      formatter: (ps) => {
        const s = samples[ps[0].dataIndex];
        if (!s) return '';
        return `<div class="tt"><h4>${hhmm(s.t)} Uhr</h4>` +
          ttRow(c.pv, 'Produktion', powerStr(s.pv)) +
          ttRow(c.load, 'Verbrauch', powerStr(s.load)) +
          ttRow(c.self, 'PV direkt genutzt', powerStr(s.self)) +
          (s.grid >= 0 ? ttRow(c.grid, 'Netzbezug', powerStr(s.grid)) : ttRow(c.pv, 'Einspeisung', powerStr(-s.grid))) +
          '</div>';
      },
    },
    xAxis: {
      type: 'value', min: 0, max: 86400, interval: $('chart').clientWidth < 520 ? 14400 : 7200,
      axisLine: { lineStyle: { color: c.border } }, splitLine: { show: false },
      axisLabel: { color: c.text3, formatter: (v) => Math.round(v / 3600) + ':00' },
    },
    yAxis: {
      type: 'value', name: 'kW', nameTextStyle: { color: c.text3, align: 'right' },
      splitLine: { lineStyle: { color: c.border, opacity: .7 } },
      axisLabel: { color: c.text3, formatter: (v) => NF1.format(v / 1000).replace(/,0$/, '') },
    },
    series: [
      { name: 'Produktion', type: 'line', data: pts(s => s.pv), showSymbol: false, sampling: 'lttb',
        lineStyle: { width: 2, color: c.pv }, itemStyle: { color: c.pv }, areaStyle: { color: c.pv, opacity: .16 }, z: 2 },
      { name: 'PV direkt genutzt', type: 'line', data: pts(s => s.self), showSymbol: false, sampling: 'lttb',
        lineStyle: { width: 0 }, itemStyle: { color: c.self }, areaStyle: { color: c.self, opacity: .38 }, z: 3 },
      { name: 'Verbrauch', type: 'line', data: pts(s => s.load), showSymbol: false, sampling: 'lttb',
        lineStyle: { width: 2, color: c.load }, itemStyle: { color: c.load }, z: 4 },
      { name: 'Netz', type: 'line', data: pts(s => s.grid), showSymbol: false, sampling: 'lttb',
        lineStyle: { width: 1.5, color: c.grid, type: [4, 3] }, itemStyle: { color: c.grid }, z: 5,
        markLine: { silent: true, symbol: 'none', lineStyle: { color: c.text3, width: 1 }, label: { show: false }, data: [{ yAxis: 0 }] } },
    ],
  });
  if (!samples.length) opt.title = emptyTitle(c);
  setChart(opt);
  $('chart-note').textContent = 'Leistung im Tagesverlauf. Netz: positiv = Bezug, negativ = Einspeisung.';
}

function setChart(opt) {
  if (!window.echarts) { $('chart').innerHTML = '<div class="empty">Diagrammbibliothek fehlt (libs/echarts.min.js)</div>'; return; }
  if (!chart) {
    chart = echarts.init($('chart'), null, { renderer: 'canvas' });
    new ResizeObserver(() => chart && chart.resize()).observe($('chart'));
  }
  chart.setOption(opt, true);
}

/* ---------- Tabelle ---------- */
function renderTable(rowLabels, entries, firstCol) {
  const head = `<tr><th>${firstCol}</th><th>Produktion</th><th>Verbrauch</th><th>PV direkt</th><th>Netzbezug</th><th>Einspeisung</th><th>Eigenverbr.</th><th>Autarkie</th></tr>`;
  const row = (label, e) => {
    if (!e) return `<tr><td>${label}</td><td colspan="7" style="text-align:center;color:var(--text-3)">–</td></tr>`;
    const r = rates(e);
    return `<tr><td>${label}</td><td>${energyStr(e.prod)}</td><td>${energyStr(e.cons)}</td><td>${energyStr(e.direct)}</td>` +
      `<td>${energyStr(e.imp)}</td><td>${energyStr(e.exp)}</td><td>${pctStr(r.selfRate)}</td><td>${pctStr(r.autarky)}</td></tr>`;
  };
  const sum = sumEntries(entries);
  $('table').innerHTML = `<table class="data"><thead>${head}</thead><tbody>${entries.map((e, i) => row(rowLabels[i], e)).join('')}</tbody>` +
    (sum ? `<tfoot>${row('Summe', sum).replace('<tr>', '<tr>')}</tfoot>` : '') + '</table>';
}

/* ==========================================================================
   Ansichten
   ========================================================================== */
let renderSeq = 0;
async function render(force = false) {
  const seq = ++renderSeq;
  const d = state.date, y = d.getFullYear(), m = d.getMonth() + 1;
  const t = today();
  updateNav();
  try {
    if (state.view === 'day') {
      const [samples, days] = await Promise.all([loadDaySamples(d, force), loadMonth(y, m, force)]);
      if (seq !== renderSeq) return;
      const hours = integrateHours(samples);
      const dayStat = days[d.getDate() - 1];
      renderKpis(dayStat || sumEntries(samples.length ? hours : []), dayStat ? null : 'aus Messwerten berechnet');
      renderDayChart(samples, d);
      renderTable(hours.map((_, h) => pad2(h) + ':00–' + pad2(h + 1) + ':00'),
                  hours.map(h => (h.prod || h.cons) ? h : null), 'Stunde (Näherung)');
    } else if (state.view === 'month') {
      const days = await loadMonth(y, m, force);
      if (seq !== renderSeq) return;
      renderKpis(sumEntries(days));
      const labels = days.map((_, i) => String(i + 1));
      const titles = days.map((_, i) => longDate(new Date(y, m - 1, i + 1)));
      renderBars(labels, days, titles);
      renderTable(titles, days, 'Tag');
    } else if (state.view === 'year') {
      const months = await loadYear(y, force);
      if (seq !== renderSeq) return;
      renderKpis(sumEntries(months));
      renderBars(MONTHS_SHORT, months, MONTHS.map(n => n + ' ' + y));
      renderTable(MONTHS.map(n => n + ' ' + y), months, 'Monat');
    } else {
      const [years, total] = await Promise.all([loadYears(force), loadTotal(force)]);
      if (seq !== renderSeq) return;
      const first = years.length ? years[0].sdi.year : (total && total.from ? total.from.year : t.getFullYear());
      const list = [], labels = [];
      for (let yy = first; yy <= t.getFullYear(); yy++) {
        labels.push(String(yy));
        list.push(years.find(e => e.sdi.year === yy) || null);
      }
      state.firstYear = first;
      const fromOk = total && total.from && total.from.year === first;
      $('period-label').dataset.total = $('period-label').textContent = fromOk
        ? `seit ${total.from.day}. ${MONTHS_SHORT[total.from.month - 1]} ${total.from.year}` : `seit ${first}`;
      // Summe der Jahre (zuverlässiger als barType=3, das ohne ORDER BY abfragt)
      renderKpis(sumEntries(list) || (total ? total.total : null));
      renderBars(labels, list, labels.map(l => 'Jahr ' + l));
      renderTable(labels, list, 'Jahr');
    }
  } catch (e) {
    if (seq !== renderSeq) return;
    renderKpis(null);
    $('chart-note').textContent = 'Fehler beim Laden: ' + e.message;
  }
}

function updateNav() {
  const d = state.date, t = today();
  const v = state.view;
  for (const id of ['btn-prev', 'btn-next', 'btn-today']) $(id).style.display = v === 'total' ? 'none' : '';
  $('date-pick').style.display = v === 'day' ? '' : 'none';
  let label = '', atEnd = false;
  if (v === 'day') { label = innerWidth < 430 ? WEEKDAYS[d.getDay()] + ', ' + fmtDate(d) : longDate(d); atEnd = sameDay(d, t); }
  else if (v === 'month') { label = MONTHS[d.getMonth()] + ' ' + d.getFullYear(); atEnd = d.getFullYear() === t.getFullYear() && d.getMonth() === t.getMonth(); }
  else if (v === 'year') { label = String(d.getFullYear()); atEnd = d.getFullYear() === t.getFullYear(); }
  $('period-label').textContent = v === 'total' ? ($('period-label').dataset.total || 'Gesamt') : label;
  $('btn-next').disabled = atEnd;
  $('btn-today').disabled = atEnd;
  $('date-input').value = isoDate(d);
  $('date-input').max = isoDate(t);
  document.querySelectorAll('.seg button').forEach(b => b.setAttribute('aria-selected', String(b.dataset.view === v)));
}

function step(dir) {
  const d = state.date;
  let n;
  if (state.view === 'day') n = new Date(d.getFullYear(), d.getMonth(), d.getDate() + dir);
  else if (state.view === 'month') n = new Date(d.getFullYear(), d.getMonth() + dir, 1);
  else n = new Date(d.getFullYear() + dir, 0, 1);
  if (n > today()) n = today();
  state.date = n;
  render();
}

/** Ansicht enthält "heute" -> regelmäßig aktualisieren */
function viewIncludesToday() {
  const d = state.date, t = today();
  if (state.view === 'day') return sameDay(d, t);
  if (state.view === 'month') return d.getFullYear() === t.getFullYear() && d.getMonth() === t.getMonth();
  if (state.view === 'year') return d.getFullYear() === t.getFullYear();
  return true;
}

/* ==========================================================================
   Start
   ========================================================================== */
async function init() {
  document.querySelectorAll('[data-icon]').forEach(el => { el.innerHTML = ICONS[el.dataset.icon] || ''; });
  document.querySelectorAll('[data-icon-svg]').forEach(g => {
    g.innerHTML = ICONS[g.dataset.iconSvg].replace('<svg ', '<svg width="24" height="24" ');
  });
  $('brand-logo').innerHTML = LOGO_SVG.replace('<svg ', '<svg width="28" height="28" ');

  // Session prüfen
  try {
    const s = await api('checksession');
    if (s.session !== 1) { toLogin(); return; }
    $('uname').textContent = s.uname || '';
    $('avatar').textContent = (s.uname || '?').charAt(0).toUpperCase();
  } catch (e) { toLogin(); return; }

  $('btn-logout').addEventListener('click', async () => {
    try { await api('logout'); } catch (e) { /* trotzdem abmelden */ }
    toLogin();
  });
  $('btn-theme').addEventListener('click', toggleTheme);
  document.addEventListener('themechange', () => render());
  matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => render());

  document.querySelectorAll('.seg button').forEach(b => b.addEventListener('click', () => {
    state.view = b.dataset.view;
    render();
  }));
  $('btn-prev').addEventListener('click', () => step(-1));
  $('btn-next').addEventListener('click', () => step(1));
  $('btn-today').addEventListener('click', () => { state.date = today(); render(); });
  $('date-input').addEventListener('change', (ev) => {
    const v = ev.target.value;
    if (!v) return;
    const [yy, mm, dd] = v.split('-').map(Number);
    state.date = new Date(yy, mm - 1, dd);
    if (state.date > today()) state.date = today();
    render();
  });
  $('btn-table').addEventListener('click', () => {
    state.showTable = !state.showTable;
    $('table').classList.toggle('hidden', !state.showTable);
    $('btn-table').setAttribute('aria-expanded', String(state.showTable));
  });
  document.addEventListener('keydown', (ev) => {
    if (ev.target.closest('input')) return;
    if (ev.key === 'ArrowLeft') step(-1);
    if (ev.key === 'ArrowRight' && !$('btn-next').disabled) step(1);
  });

  document.addEventListener('visibilitychange', () => {
    if (!document.hidden) { pollLive(); }
  });

  watchFlowSize();
  pollLive();
  updateTodaySummary();
  render();

  setInterval(() => {
    if (document.hidden) return;
    updateTodaySummary(true);
    if (viewIncludesToday()) render(true);
  }, REFRESH_INTERVAL);
}

document.addEventListener('DOMContentLoaded', init);
