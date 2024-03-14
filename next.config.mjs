import createNextIntlPlugin from "next-intl/plugin";
import { join } from "path";

const withNextIntl = createNextIntlPlugin();

const IS_DEVELOPMENT = process.env.NODE_ENV === "development";

/** @type {import('next').NextConfig} */
const nextConfig = {
  poweredByHeader: IS_DEVELOPMENT,
  sassOptions: {
    includePaths: [join(process.cwd(), "src/app/scss")],
    sourceMap: true,
  },
  images: {
    remotePatterns: [
      {
        protocol: "http",
        hostname: "localhost",
        port: "9001",
      },
      {
        protocol: "http",
        hostname: "localhost",
        port: "4000",
      },
      {
        protocol: "https",
        hostname: "lh3.googleusercontent.com",
      },
    ],
  },
  webpack: (config, options) => {
    const fileLoaderRule = config.module.rules.find((rule) =>
      rule.test?.test?.(".svg")
    );

    config.module.rules.push(
      {
        ...fileLoaderRule,
        test: /\.svg$/i,
        resourceQuery: /url/, // *.svg?url
      },

      // Convert all other *.svg imports to React components
      {
        test: /\.svg$/i,
        issuer: fileLoaderRule.issuer,
        resourceQuery: { not: [...fileLoaderRule.resourceQuery.not, /url/] }, // exclude if *.svg?url
        use: ["@svgr/webpack"],
      }
    );

    // Modify the file loader rule to ignore *.svg, since we have it handled now.
    fileLoaderRule.exclude = /\.svg$/i;

    /**
     * Force scss source maps for debugging. If there are performance issues or you don't need debug css, use the value "eval-source-map" instead.
     */
    if (IS_DEVELOPMENT && options.dev) {
      Object.defineProperty(config, "devtool", {
        get() {
          return "source-map";
        },
        set() {},
      });
    }

    return config;
  },
};

export default withNextIntl(nextConfig);
