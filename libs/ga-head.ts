// the analytics include of every notebook (_quarto.yml pre-render, run by quarto for a whole render and for one file):
// writes libs/ga-head.html (git-ignored) from GA_ID in the render shell. unset or malformed: an include with no
// script, so nothing loads. set: @oceanmetrics/nav's own loader (the one the Ocean Metrics bar runs on every page:
// anonymize_ip, no Google signals, no ad personalization, a page_location of origin + path, never the query or the
// fragment; nav README "Analytics"), from jsDelivr at a pinned version. no id is ever written in the repo.
const NAV = "0.2.0";
const GA_ID_RE = /^G-[A-Z0-9]{4,16}$/;

export function gaHead(raw: string | undefined): string {
  const id = (raw ?? "").trim();
  if (!GA_ID_RE.test(id)) return "<!-- analytics off: no GA_ID at render (libs/ga-head.ts) -->\n";
  return `<!-- analytics: GA_ID at render (libs/ga-head.ts), the Ocean Metrics loader of @oceanmetrics/nav ${NAV} -->
<script type="module">
import { installGa } from "https://cdn.jsdelivr.net/npm/@oceanmetrics/nav@${NAV}/dist/ga.js";
installGa("${id}", { anonymize: true, pageView: "load" });
</script>
`;
}

if (import.meta.main) Deno.writeTextFileSync(new URL("./ga-head.html", import.meta.url), gaHead(Deno.env.get("GA_ID")));
