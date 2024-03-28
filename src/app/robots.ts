import { MetadataRoute } from 'next'
import { headers } from 'next/headers'

export default function robots(): MetadataRoute.Robots {
  const header = headers()
  const host = header.get('host') ?? ''
  const protocol = header.get('x-forwarded-proto') ?? 'http'

  return {
    rules: {
      userAgent: '*',
      allow: '/',
      disallow: ['/api/', '/backend/'],
    },
    sitemap: `${protocol}://${host}/sitemap.xml`,
  }
}
