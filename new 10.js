// @see https://leerob.io/blog/nextjs-sitemap-robots#external-content
import { writeFileSync } from "fs";
import globby from "globby";
import prettier from "prettier";

async function generateSitemap() {
  const date = new Date();
  const URL = "https://trans-hands.com";
  const prettierConfig = await prettier.resolveConfig("./.prettierrc.js");
  const pages = await globby([
    "pages/**/*.tsx",
    "!pages/_*.tsx",
    "!pages/api",
    "!pages/404.tsx",
    "!pages/500.tsx",
  ]);

  const sitemap = `
    <?xml version="1.0" encoding="UTF-8"?>
    <urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:xhtml="http://www.w3.org/1999/xhtml">
        ${pages
          .map((page) => {
            const route = page
              .replace("pages", "")
              .replace("data", "")
              .replace(".tsx", "")
              .replace(".mdx", "")
              .replace("/index", "");
            return `
            <url>
              <changefreq>weekly</changefreq>
              <priority>0.7</priority>
              <lastmod>${date.toISOString()}</lastmod>
              <loc>${`${URL}${route}`}</loc>
              <xhtml:link rel="alternate" hreflang="es" href="${`${URL}/es${route}`}"/>
              <xhtml:link rel="alternate" hreflang="ca" href="${`${URL}/ca${route}`}"/>
            </url>
            `;
          })
          .join("")}
    </urlset>
    `;

  const formatted = prettier.format(sitemap, {
    ...prettierConfig,
    parser: "html",
  });

  writeFileSync("public/sitemap.xml", formatted);
}

generateSitemap();