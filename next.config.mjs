import createNextIntlPlugin from "next-intl/plugin";
import { join } from "path";

const withNextIntl = createNextIntlPlugin();

/** @type {import('next').NextConfig} */
const nextConfig = {
  poweredByHeader: false,
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
    /**
     * Force scss source maps for debugging. If there are performance issues or you don't need debug css, use the value "eval-source-map" instead.
     */
    if (process.env.NODE_ENV === "development" && options.dev) {
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
