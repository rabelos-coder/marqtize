import createNextIntlPlugin from 'next-intl/plugin'
import nextPWA from 'next-pwa'
import { join } from 'path'

const withNextIntl = createNextIntlPlugin()

const IS_DEVELOPMENT = process.env.NODE_ENV === 'development'

const withPWA = nextPWA({
  dest: 'public',
  disable: IS_DEVELOPMENT,
})

/** @type {import('next').NextConfig} */
const nextConfig = {
  poweredByHeader: false,
  generateEtags: false,
  reactStrictMode: true,
  swcMinify: true,
  images: {
    remotePatterns: [
      {
        protocol: 'http',
        hostname: 'localhost',
        port: '9001',
      },
      {
        protocol: 'http',
        hostname: 'localhost',
        port: '4000',
      },
      {
        protocol: 'https',
        hostname: 'lh3.googleusercontent.com',
      },
      {
        protocol: 'https',
        hostname: 'scontent.faqa2-1.fna.fbcdn.net',
      },
    ],
  },
}

if (IS_DEVELOPMENT) {
  nextConfig['sassOptions'] = {
    includePaths: [join(process.cwd(), 'src/app/scss')],
    sourceMap: true,
  }
  nextConfig['webpack'] = (config) => {
    /**
     * Force scss source maps for debugging. If there are performance issues or you don't need debug css, use the value "eval-source-map" instead.
     */
    Object.defineProperty(config, 'devtool', {
      get() {
        return 'source-map'
      },
      set() {},
    })

    return config
  }
}

export default withNextIntl(withPWA(nextConfig))
