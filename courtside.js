/* ---------------------------------------------------------------------------
   Courtside — behaviour for the "Meet the WNBA" page.

   Everything is driven by one JSON payload written into the page by R (see the
   `payload` chunk in dashboard.qmd). Nothing is fetched at view time and there
   is no charting library: the race chart is SVG built here, the bars and
   meters are divs. That keeps the whole page a single ~1 MB file that opens
   instantly, and it lets the charts share the page's type and colour rather
   than arriving with a library's own defaults.

   The connective idea: a player picked anywhere — wall, leaderboard, race
   legend — becomes the selected player everywhere.
--------------------------------------------------------------------------- */
(function () {
  "use strict";

  var D = JSON.parse(document.getElementById("payload").textContent);
  var P = D.players;
  var byName = {};
  P.forEach(function (p) { byName[p.name] = p; });

  var $ = function (s, r) { return (r || document).querySelector(s); };
  var $$ = function (s, r) { return Array.prototype.slice.call((r || document).querySelectorAll(s)); };
  var esc = function (s) {
    return String(s).replace(/[&<>"]/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c];
    });
  };
  var f1 = function (x) { return (x === null || x === undefined) ? "—" : x.toFixed(1); };
  var ord = function (n) {
    if (!n) return "";
    var s = ["th", "st", "nd", "rd"], v = n % 100;
    return n + (s[(v - 20) % 10] || s[v] || s[0]);
  };

  /* -------------------------------------------------------------------------
     Shared tooltip
  ------------------------------------------------------------------------- */
  var tip = document.getElementById("tip");
  function showTip(e, l1, l2) {
    tip.innerHTML = esc(l1) + (l2 ? '<span class="t2">' + esc(l2) + "</span>" : "");
    tip.classList.add("on");
    moveTip(e);
  }
  function moveTip(e) {
    var w = tip.offsetWidth;
    tip.style.left = Math.min(Math.max(8, e.clientX - w / 2), window.innerWidth - w - 8) + "px";
    tip.style.top = (e.clientY - tip.offsetHeight - 12) + "px";
  }
  function hideTip() { tip.classList.remove("on"); }

  /* -------------------------------------------------------------------------
     Page furniture — scroll progress, section rail, reveal on scroll
  ------------------------------------------------------------------------- */
  var bar = document.getElementById("progress");
  var railLinks = $$("#rail a");
  var bands = railLinks.map(function (a) { return document.querySelector(a.getAttribute("href")); });

  function onScroll() {
    var h = document.documentElement.scrollHeight - window.innerHeight;
    bar.style.width = (h > 0 ? (window.scrollY / h) * 100 : 0) + "%";
    var mid = window.scrollY + window.innerHeight * 0.35, active = 0;
    bands.forEach(function (b, i) { if (b && b.offsetTop <= mid) active = i; });
    railLinks.forEach(function (a, i) { a.classList.toggle("on", i === active); });
  }
  window.addEventListener("scroll", onScroll, { passive: true });
  onScroll();

  /* `top < 0` as well as isIntersecting: someone who scrolls faster than the
     observer reports would otherwise leave a band's heading and controls
     stranded at opacity 0, above the viewport, with nothing left to trigger
     them. Landing on a #fragment does the same thing. */
  function arrived(r) { return r.isIntersecting || r.boundingClientRect.top < 0; }

  var io = new IntersectionObserver(function (rows) {
    rows.forEach(function (r) {
      if (!arrived(r)) return;
      r.target.classList.add("in");
      io.unobserve(r.target);
    });
  }, { rootMargin: "-40px" });
  $$(".reveal").forEach(function (el) { io.observe(el); });

  /* Count the hero figures up once they are on screen. */
  var cio = new IntersectionObserver(function (rows) {
    rows.forEach(function (r) {
      if (!arrived(r)) return;
      cio.unobserve(r.target);
      var el = r.target, end = +el.dataset.to, t0 = null;
      if (!r.isIntersecting) { el.textContent = end.toLocaleString(); return; }
      function step(t) {
        if (t0 === null) t0 = t;
        var k = Math.min(1, (t - t0) / 900);
        el.textContent = Math.round(end * (1 - Math.pow(1 - k, 3))).toLocaleString();
        if (k < 1) requestAnimationFrame(step);
      }
      requestAnimationFrame(step);
    });
  });
  $$("[data-to]").forEach(function (el) { cio.observe(el); });

  /* -------------------------------------------------------------------------
     The wall — every player in the league, filterable
  ------------------------------------------------------------------------- */
  var wall = document.getElementById("wall");
  var more = document.getElementById("more");
  var state = { q: "", team: "", rookies: false, sort: "pts", all: false };
  var SORTS = { pts: "ppg", reb: "rpg", ast: "apg", min: "mpg" };
  var FIRST = 48;

  function visible() {
    var q = state.q.toLowerCase();
    var out = P.filter(function (p) {
      if (state.team && p.team !== state.team) return false;
      if (state.rookies && !p.rook) return false;
      if (q && p.name.toLowerCase().indexOf(q) === -1 && p.teamName.toLowerCase().indexOf(q) === -1) return false;
      return true;
    });
    if (state.sort === "az") {
      out.sort(function (a, b) { return a.last.localeCompare(b.last) || a.first.localeCompare(b.first); });
    } else {
      /* Sorting a wall by a per-game average would otherwise hand the top row
         to whoever played twice and had one good night, so the players with a
         real sample come first and the cameos follow. */
      var k = SORTS[state.sort];
      out.sort(function (a, b) {
        return (b.g >= D.minGames) - (a.g >= D.minGames) || b[k] - a[k];
      });
    }
    return out;
  }

  function headline(p) {
    var k = state.sort === "az" ? "pts" : state.sort;
    var unit = { pts: "PPG", reb: "RPG", ast: "APG", min: "MIN" }[k];
    return { v: f1(p[SORTS[k]]), u: unit };
  }

  /* Native loading="lazy" fetches far more of a 225-card wall than is on
     screen, and ESPN throttles once a couple of hundred requests are in
     flight — the wall then sits half-blank for a minute. Holding the URL in
     data-src and assigning it from an observer keeps roughly a screenful in
     flight at a time. */
  var shotIO = new IntersectionObserver(function (rows) {
    rows.forEach(function (r) {
      if (!r.isIntersecting) return;
      shotIO.unobserve(r.target);
      r.target.src = r.target.dataset.src;
    });
  }, { rootMargin: "400px 0px" });

  function cardHTML(p) {
    var h = headline(p);
    var shot = p.shot
      ? '<img class="shot" data-src="' + p.shot + '" alt="" decoding="async">'
      : '<div class="noshot">' + esc(p.initials) + "</div>";
    return '<button class="pcard" type="button" style="--team:' + p.color + '" data-n="' + esc(p.name) + '">' +
      (p.jersey ? '<span class="jersey">' + esc(p.jersey) + "</span>" : "") +
      shot +
      '<span class="meta">' +
        '<span class="nm">' + esc(p.name) + "</span>" +
        '<span class="sub">' + esc(p.team) + (p.pos ? " · " + esc(p.pos) : "") + (p.rook ? " · ROOKIE" : "") + "</span>" +
        '<span class="headline"><span class="v">' + h.v + '</span><span class="u">' + h.u + "</span></span>" +
      "</span></button>";
  }

  function renderWall() {
    var list = visible();
    var shown = state.all ? list : list.slice(0, FIRST);
    wall.innerHTML = list.length
      ? shown.map(cardHTML).join("")
      : '<div class="empty">No player by that name</div>';

    more.hidden = list.length <= FIRST;
    more.textContent = state.all
      ? "Show fewer"
      : "Show all " + list.length + " players";
    document.getElementById("wall-count").textContent = shown.length < list.length
      ? shown.length + " of " + list.length + " players"
      : list.length + (list.length === 1 ? " player" : " players");

    $$("#wall img[data-src]").forEach(function (im) { shotIO.observe(im); });
    markSelected();
  }

  more.addEventListener("click", function () {
    state.all = !state.all;
    renderWall();
    if (!state.all) document.getElementById("who").scrollIntoView({ block: "start" });
  });

  wall.addEventListener("click", function (e) {
    var c = e.target.closest(".pcard");
    if (c) select(c.dataset.n);
  });

  document.getElementById("search").addEventListener("input", function (e) {
    state.q = e.target.value; renderWall();
  });

  $$("#wall-sort button").forEach(function (b) {
    b.addEventListener("click", function () {
      state.sort = b.dataset.sort;
      $$("#wall-sort button").forEach(function (o) { o.classList.toggle("on", o === b); });
      renderWall();
    });
  });

  $$(".chip").forEach(function (c) {
    c.addEventListener("click", function () {
      if (c.dataset.rookies !== undefined) { state.rookies = !state.rookies; state.team = ""; }
      else { state.team = c.dataset.team || ""; state.rookies = false; }
      syncChips();
      renderWall();
    });
  });

  function syncChips() {
    $$(".chip").forEach(function (c) {
      var on = c.dataset.rookies !== undefined
        ? state.rookies
        : (!state.rookies && (c.dataset.team || "") === state.team);
      c.classList.toggle("on", on);
    });
  }

  function markSelected() {
    $$(".pcard").forEach(function (c) { c.classList.toggle("on", c.dataset.n === current); });
  }

  /* -------------------------------------------------------------------------
     The dossier — one player, in full
  ------------------------------------------------------------------------- */
  var current = null;
  var dos = document.getElementById("dossier");

  function meter(label, val, avg, max) {
    if (val === null) {
      return '<div class="meter"><div class="top"><span>' + label + '</span><span class="val">—</span></div>' +
        '<div class="track"></div></div>';
    }
    return '<div class="meter">' +
      '<div class="top"><span>' + label + '</span><span class="val">' + val.toFixed(1) + "%</span></div>" +
      '<div class="track"><div class="fill" style="width:' + Math.min(100, (val / max) * 100) + '%"></div>' +
      '<div class="avg" style="left:' + Math.min(100, (avg / max) * 100) + '%"></div></div></div>';
  }

  /* One plain sentence, because a rank means nothing to a new viewer until it
     is put next to the size of the league. */
  function verdict(p) {
    var n = D.qualified;
    var bits = [];
    if (p.rk_ppg) bits.push("scores <b>" + f1(p.ppg) + "</b> points a game, " + ord(p.rk_ppg) + " of " + n + " regulars");
    else bits.push("has played <b>" + p.g + "</b> game" + (p.g === 1 ? "" : "s") + " this season");
    var best = null;
    [["rk_rpg", "rebounding", p.rpg, "rebounds"], ["rk_apg", "passing", p.apg, "assists"],
     ["rk_bpg", "shot-blocking", p.bpg, "blocks"], ["rk_spg", "ball-stealing", p.spg, "steals"]]
      .forEach(function (r) { if (p[r[0]] && p[r[0]] <= 15 && (!best || p[r[0]] < p[best[0]])) best = r; });
    if (best) bits.push("and ranks " + ord(p[best[0]]) + " in " + best[1] + " at <b>" + f1(best[2]) + "</b> " + best[3] + " a game");
    else if (p.dd > 0) bits.push("with <b>" + p.dd + "</b> double-double" + (p.dd === 1 ? "" : "s"));
    return p.first + " " + bits.join(", ") + ".";
  }

  function logHTML(p) {
    if (!p.gl.length) return "";
    var mx = Math.max.apply(null, p.gl.map(function (g) { return g.p; })) || 1;
    var bars = p.gl.map(function (g, i) {
      return '<div class="g' + (g.w ? "" : " lose") + '" style="height:' +
        Math.max(3, (g.p / mx) * 100) + '%" data-i="' + i + '"></div>';
    }).join("");
    return '<div class="dos-sec"><h4>Every game this season · points</h4>' +
      '<div class="gamelog" id="gamelog">' + bars + "</div>" +
      '<div class="gamelog-key"><span>' + esc(p.gl[0].d) + '</span>' +
      '<span>best night · ' + mx + '</span>' +
      '<span>' + esc(p.gl[p.gl.length - 1].d) + "</span></div></div>";
  }

  function cell(v, k, rk) {
    return '<div class="cell"><span class="v">' + v + '</span><span class="k">' + k + "</span>" +
      (rk ? '<span class="rk">' + ord(rk) + "</span>" : "") + "</div>";
  }

  function renderDossier(p) {
    dos.style.setProperty("--team", p.color);
    dos.innerHTML =
      '<div class="dos-top">' +
        (p.shot ? '<img class="dos-shot" src="' + p.shot + '" alt="">' : '<div class="dos-shot"></div>') +
        '<div class="dos-id">' +
          '<h3 class="dos-name">' + esc(p.name) + "</h3>" +
          '<div class="dos-team"><img src="' + p.logo + '" alt="">' + esc(p.teamName) +
            (p.jersey ? " · #" + esc(p.jersey) : "") + (p.pos ? " · " + esc(p.pos) : "") + "</div>" +
        "</div>" +
      "</div>" +
      '<p class="dos-verdict">' + verdict(p) + "</p>" +
      '<div class="dos-grid">' +
        cell(f1(p.ppg), "pts / gm", p.rk_ppg) +
        cell(f1(p.rpg), "reb / gm", p.rk_rpg) +
        cell(f1(p.apg), "ast / gm", p.rk_apg) +
        cell(String(p.g), "games", 0) +
      "</div>" +
      '<div class="dos-sec"><h4>Shooting · white tick is the league</h4>' +
        meter("Field goals", p.fgp, D.avg.fgp, 65) +
        meter("Three-pointers", p.tpp, D.avg.tpp, 50) +
        meter("Free throws", p.ftp, D.avg.ftp, 100) +
      "</div>" +
      logHTML(p) +
      '<div class="dos-hint">' + p.dd + " double-double" + (p.dd === 1 ? "" : "s") +
        (p.td ? " · " + p.td + " triple-double" + (p.td === 1 ? "" : "s") : "") +
        " · " + p.min + " minutes played</div>";

    var gl = document.getElementById("gamelog");
    if (gl) {
      gl.addEventListener("mousemove", function (e) {
        var b = e.target.closest(".g");
        if (!b) { hideTip(); return; }
        var g = p.gl[+b.dataset.i];
        showTip(e, g.p + " points", g.d + " vs " + g.o + " · " + (g.w ? "won" : "lost"));
      });
      gl.addEventListener("mouseleave", hideTip);
    }
  }

  function select(name, scroll) {
    var p = byName[name];
    if (!p) return;
    current = name;
    renderDossier(p);
    /* Someone picked from a leaderboard or from the race may be sitting past
       the first rows of the wall, so open the rest rather than highlight a
       card that is not on the page. */
    if (!state.all && visible().slice(0, FIRST).indexOf(p) === -1) {
      state.all = true;
      renderWall();
    } else {
      markSelected();
    }
    drawRace();
    if (scroll) document.getElementById("who").scrollIntoView({ block: "start" });
  }

  /* -------------------------------------------------------------------------
     Leaderboards
  ------------------------------------------------------------------------- */
  var METRICS = [
    { id: "pts", label: "Points", tot: "pts", per: "ppg" },
    { id: "reb", label: "Rebounds", tot: "reb", per: "rpg" },
    { id: "ast", label: "Assists", tot: "ast", per: "apg" },
    { id: "stl", label: "Steals", tot: "stl", per: "spg" },
    { id: "blk", label: "Blocks", tot: "blk", per: "bpg" },
    { id: "tpm", label: "3-Pointers", tot: "tpm", per: "tpg" },
    { id: "dd", label: "Double-Doubles", tot: "dd", per: null }
  ];
  var metric = METRICS[0], perGame = false;
  var board = document.getElementById("board");

  function renderBoard() {
    var key = (perGame && metric.per) ? metric.per : metric.tot;
    var pool = (perGame && metric.per) ? P.filter(function (p) { return p.g >= D.minGames; }) : P;
    var top = pool.slice().sort(function (a, b) { return b[key] - a[key]; }).slice(0, 10);
    var mx = top.length ? top[0][key] : 1;

    board.innerHTML = top.map(function (p, i) {
      var v = (perGame && metric.per) ? f1(p[key]) : String(p[key]);
      return '<div class="lrow" style="--team:' + p.color + '" data-n="' + esc(p.name) + '">' +
        '<span class="rk">' + (i + 1) + "</span>" +
        (p.shot ? '<img class="av" src="' + p.shot + '" alt="" loading="lazy">' : '<span class="av"></span>') +
        '<span class="bar-wrap"><span class="lbl">' +
          '<span class="nm">' + esc(p.name) + '<span class="tm">' + esc(p.team) + "</span></span>" +
          '<span class="vl">' + v + "</span></span>" +
        '<span class="track"><span class="fill"></span></span></span></div>';
    }).join("");

    requestAnimationFrame(function () {
      $$("#board .fill").forEach(function (f, i) {
        f.style.width = (mx ? (top[i][key] / mx) * 100 : 0) + "%";
      });
    });
    document.getElementById("board-note").textContent = (perGame && metric.per)
      ? "Per game, among the " + D.qualified + " players with at least " + D.minGames + " games."
      : "Season totals, every player.";
  }

  board.addEventListener("click", function (e) {
    var r = e.target.closest(".lrow");
    if (r) select(r.dataset.n, true);
  });

  $$("#lead-tabs button").forEach(function (b) {
    b.addEventListener("click", function () {
      metric = METRICS.filter(function (m) { return m.id === b.dataset.m; })[0];
      $$("#lead-tabs button").forEach(function (o) { o.classList.toggle("on", o === b); });
      document.getElementById("per-toggle").hidden = !metric.per;
      renderBoard();
    });
  });

  $$("#per-toggle button").forEach(function (b) {
    b.addEventListener("click", function () {
      perGame = b.dataset.per === "1";
      $$("#per-toggle button").forEach(function (o) { o.classList.toggle("on", o === b); });
      renderBoard();
    });
  });

  /* -------------------------------------------------------------------------
     The record book — the same bar rows, over careers rather than a season.
     Players still active are marked, so a newcomer can tell which of these
     totals are finished and which are still moving.
  ------------------------------------------------------------------------- */
  var careerBoard = document.getElementById("career-board");
  var careerMetric = "pts";

  function renderCareer() {
    var rows = D.career[careerMetric];
    var mx = rows.length ? rows[0].v : 1;
    careerBoard.innerHTML = rows.map(function (p, i) {
      var face = p.shot
        ? '<img class="av" src="' + p.shot + '" alt="" loading="lazy">'
        : '<span class="av av-initials">' + esc(p.initials) + "</span>";
      return '<div class="lrow' + (p.active ? " live" : "") + '" style="--team:' + p.color + '">' +
        '<span class="rk">' + (i + 1) + "</span>" + face +
        '<span class="bar-wrap"><span class="lbl">' +
          '<span class="nm">' + esc(p.name) + '<span class="tm">' + p.span +
            (p.trunc ? " &dagger;" : "") + "</span></span>" +
          '<span class="vl">' + p.v.toLocaleString() + "</span></span>" +
        '<span class="track"><span class="fill"></span></span></span></div>';
    }).join("");
    requestAnimationFrame(function () {
      $$("#career-board .fill").forEach(function (f, i) {
        f.style.width = (mx ? (rows[i].v / mx) * 100 : 0) + "%";
      });
    });
  }

  $$("#career-tabs button").forEach(function (b) {
    b.addEventListener("click", function () {
      careerMetric = b.dataset.m;
      $$("#career-tabs button").forEach(function (o) { o.classList.toggle("on", o === b); });
      renderCareer();
    });
  });

  /* Someone in the record book who is still playing has a dossier to show. */
  careerBoard.addEventListener("click", function (e) {
    var r = e.target.closest(".lrow.live");
    if (r) select(r.querySelector(".nm").childNodes[0].nodeValue, true);
  });

  /* -------------------------------------------------------------------------
     The race — cumulative points, drawn as SVG

     Every player in the league is a line. Eight are in their team's colour and
     carry a name; the rest are the grey pack behind them. Selecting a player
     anywhere on the page pulls their line out of the pack.
  ------------------------------------------------------------------------- */
  var svg = document.getElementById("race");
  var R = D.race;
  var VW = 1000, VH = 430, PAD = { t: 18, r: 132, b: 34, l: 46 };

  function xAt(i) { return PAD.l + (i / (R.dates.length - 1)) * (VW - PAD.l - PAD.r); }
  function yAt(v) { return VH - PAD.b - (v / R.max) * (VH - PAD.t - PAD.b); }

  function path(v) {
    var d = "", pen = false;
    for (var i = 0; i < v.length; i++) {
      if (v[i] === null) continue;
      d += (pen ? "L" : "M") + xAt(i).toFixed(1) + " " + yAt(v[i]).toFixed(1) + " ";
      pen = true;
    }
    return d;
  }

  function lastPoint(v) {
    for (var i = v.length - 1; i >= 0; i--) if (v[i] !== null) return { i: i, v: v[i] };
    return null;
  }

  function niceStep(x) {
    var e = Math.pow(10, Math.floor(Math.log10(x))), f = x / e;
    return (f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10) * e;
  }

  function drawRace() {
    var g = [], step = niceStep(R.max / 5);
    for (var y = 0; y <= R.max; y += step) {
      g.push('<line class="grid" x1="' + PAD.l + '" x2="' + (VW - PAD.r) + '" y1="' + yAt(y).toFixed(1) + '" y2="' + yAt(y).toFixed(1) + '"/>');
      g.push('<text class="axis" x="' + (PAD.l - 8) + '" y="' + (yAt(y) + 4).toFixed(1) + '" text-anchor="end">' + y + "</text>");
    }
    R.ticks.forEach(function (t) {
      g.push('<text class="axis" x="' + xAt(t.i).toFixed(1) + '" y="' + (VH - 10) + '" text-anchor="middle">' + esc(t.l) + "</text>");
    });

    var pack = [], lead = [], labels = [];
    R.series.forEach(function (s) {
      var isSel = s.n === current;
      if (!s.top && !isSel) { pack.push('<path class="pack" d="' + path(s.v) + '"/>'); return; }
      var lp = lastPoint(s.v);
      lead.push('<path class="lead' + (isSel ? " hot" : "") + '" stroke="' +
        (isSel ? "#f2ece1" : s.c) + '" d="' + path(s.v) + '" data-n="' + esc(s.n) + '"/>');
      if (lp) {
        labels.push({ y: yAt(lp.v), x: xAt(lp.i), n: s.n, c: s.c, sel: isSel });
      }
    });

    /* Nudge end labels apart so a tight top of the table stays readable. */
    labels.sort(function (a, b) { return a.y - b.y; });
    for (var i = 1; i < labels.length; i++) {
      if (labels[i].y - labels[i - 1].y < 13) labels[i].y = labels[i - 1].y + 13;
    }
    var lab = labels.map(function (l) {
      return '<text class="lead-label" x="' + (l.x + 7).toFixed(1) + '" y="' + (l.y + 4).toFixed(1) +
        '" fill="' + (l.sel ? "#f2ece1" : l.c) + '">' + esc(l.n) + "</text>";
    }).join("");

    svg.innerHTML = g.join("") + pack.join("") + lead.join("") + lab;
  }

  svg.addEventListener("mousemove", function (e) {
    var p = e.target.closest("path.lead");
    if (!p) { hideTip(); return; }
    var s = R.series.filter(function (x) { return x.n === p.dataset.n; })[0];
    var lp = lastPoint(s.v);
    showTip(e, s.n, lp.v + " points so far");
  });
  svg.addEventListener("mouseleave", hideTip);
  svg.addEventListener("click", function (e) {
    var p = e.target.closest("path.lead");
    if (p) select(p.dataset.n, true);
  });

  /* -------------------------------------------------------------------------
     Standings — a team card is also a filter for the wall
  ------------------------------------------------------------------------- */
  document.getElementById("standings").addEventListener("click", function (e) {
    var c = e.target.closest(".tcard");
    if (!c) return;
    state.team = c.dataset.team;
    state.rookies = false;
    state.q = "";
    document.getElementById("search").value = "";
    syncChips();
    renderWall();
    document.getElementById("who").scrollIntoView({ block: "start" });
  });

  /* -------------------------------------------------------------------------
     Boot
  ------------------------------------------------------------------------- */
  syncChips();
  renderWall();
  renderBoard();
  renderCareer();
  select(D.opening);
  window.addEventListener("mousemove", function (e) { if (tip.classList.contains("on")) moveTip(e); }, { passive: true });
  document.addEventListener("keydown", function (e) {
    if (e.key !== "Escape") return;
    state.q = ""; state.team = ""; state.rookies = false;
    document.getElementById("search").value = "";
    syncChips(); renderWall();
  });
})();
