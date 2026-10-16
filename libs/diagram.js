// Diagrams you can zoom into, without a lightbox.
//
// Every diagram in the book is an SVG pre-rendered from diagrams/*.mmd and embedded as
// a plain markdown figure, `![caption](diagrams/x.svg){.cc-diagram .nolightbox}`, so the
// PDF and DOCX get the static image. In HTML this script upgrades each such <img>: it
// fetches the SVG, inlines it (so the text is selectable and the id is made unique per
// page), and hands it to svg-pan-zoom with +/−/reset controls, drag to pan, and a
// fullscreen button. The mouse wheel zooms only in fullscreen, so a diagram never
// hijacks the page scroll. GLightbox was the previous answer: an SVG with no intrinsic
// size renders tiny there and the caption becomes the whole panel (Ben, 2026-09-08).
(function () {
  const INLINE_H = 560;   // px, the inline viewer's height
  let n = 0;

  function fullscreenButton(wrap, pz) {
    const b = document.createElement("button");
    b.type = "button"; b.className = "cc-diagram-full"; b.title = "Full screen (Esc to close)";
    b.setAttribute("aria-label", "Toggle full screen"); b.textContent = "⤢";
    const set = (on) => {
      wrap.classList.toggle("is-full", on);
      document.body.classList.toggle("cc-diagram-open", on);
      b.textContent = on ? "✕" : "⤢";
      if (on) pz.enableMouseWheelZoom(); else pz.disableMouseWheelZoom();
      requestAnimationFrame(() => { pz.resize(); pz.fit(); pz.center(); });
    };
    b.addEventListener("click", () => set(!wrap.classList.contains("is-full")));
    document.addEventListener("keydown", (e) => { if (e.key === "Escape" && wrap.classList.contains("is-full")) set(false); });
    return b;
  }

  async function upgrade(img) {
    const src = img.getAttribute("src");
    if (!src || !/\.svg(\?|$)/i.test(src)) return;
    let text;
    try { text = await (await fetch(src)).text(); } catch (e) { return; }   // leave the <img>
    const id = `cc-diagram-${++n}`;
    // mermaid scopes its <style> to the svg id; keep the two in step and unique per page
    text = text.replace(/my-svg/g, id)
      // mermaid writes a classDef's `color:` as an inline `fill:… !important` on every <text>,
      // which no stylesheet can override; drop it so brand-dark.scss can recolour the labels
      .replace(/ style="fill:#[0-9a-fA-F]{3,6} !important"/g, "")
      // …and every other inline `!important` (classDef fills, cluster styles): an inline style
      // still beats the stylesheet without it, so the light look is unchanged, and the dark
      // theme's !important rules can win
      .replace(/(style="[^"]*?)\s*!important/g, (m, a) => a).replace(/\s!important(?=[;"])/g, "");
    // size only the ROOT element: every rect and image inside keeps its own width/height
    text = text.replace(/^(\s*<svg\b[^>]*)>/, (m, open) =>
      open.replace(/\s(width|height)="[^"]*"/g, "") + ' width="100%" height="100%">');
    const wrap = document.createElement("div");
    wrap.className = "cc-diagram-wrap";
    wrap.style.height = INLINE_H + "px";
    wrap.innerHTML = text;
    const svg = wrap.querySelector("svg");
    if (!svg || typeof svgPanZoom !== "function") return;
    svg.setAttribute("width", "100%"); svg.setAttribute("height", "100%");
    svg.style.maxWidth = "none";
    img.replaceWith(wrap);
    const pz = svgPanZoom(svg, {
      zoomEnabled: true, controlIconsEnabled: true, fit: true, center: true,
      minZoom: 0.4, maxZoom: 40, zoomScaleSensitivity: 0.35,
      mouseWheelZoomEnabled: false, dblClickZoomEnabled: true, preventMouseEventsDefault: true,
    });
    wrap.appendChild(fullscreenButton(wrap, pz));
    // refit when the column changes width (sidebar toggles, window resize)
    if (window.ResizeObserver) new ResizeObserver(() => { pz.resize(); pz.fit(); pz.center(); }).observe(wrap);
  }

  function run() { document.querySelectorAll("img.cc-diagram").forEach(upgrade); }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", run); else run();
})();
