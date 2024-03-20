import { Metadata } from 'next/types'

import { APP_META_TITLE, APP_META_TITLE_SEPARATOR } from '@/environment'

/**
 * Concatenates the given title with the global application meta title and separator.
 *
 * @param {string} title - The title to be concatenated
 * @return {string} The concatenated title
 */
export const concatTitle = (title: string): string =>
  `${APP_META_TITLE} ${APP_META_TITLE_SEPARATOR} ${title}`

export const icons: Metadata['icons'] = [
  {
    url: '/assets/images/favicon/apple-touch-icon.png',
    rel: 'apple-touch-icon',
    sizes: '180x180',
  },
  {
    url: '/assets/images/favicon/favicon-32x32.png',
    rel: 'icon',
    sizes: '32x32',
    type: 'image/png',
  },
  {
    url: '/assets/images/favicon/favicon-16x16.png',
    rel: 'icon',
    sizes: '16x16',
    type: 'image/png',
  },
]

/**
 * Retrieves the valid subdomain from the given host.
 *
 * @param {string | null} host - The host from which to retrieve the subdomain.
 * @return {string | null} The valid subdomain, if found; otherwise, null.
 */
export const getValidSubdomain = (host?: string | null) => {
  let subdomain: string | null = null
  if (!host && typeof window !== 'undefined') {
    // On client side, get the host from window
    host = window.location.host
  }
  if (host && host.includes('.')) {
    const candidate = host.split('.')[0]
    if (candidate && !candidate.includes('localhost')) {
      // Valid candidate
      subdomain = candidate
    }
  }

  return subdomain
}

/**
 * Handles click event on an anchor element, prevents default behavior, and scrolls to the referenced element.
 *
 * @param {React.MouseEvent<HTMLAnchorElement>} e - The click event object
 * @return {void}
 */
export const anchorClick = (e: React.MouseEvent<HTMLAnchorElement>): void => {
  if (typeof e === 'object') {
    e?.preventDefault()
    document
      ?.querySelector(`${e?.currentTarget?.getAttribute('href')}`)
      ?.scrollIntoView({
        behavior: 'smooth',
      })
  }
}

/**
 * Generates a WhatsApp link with the given phone number and optional message.
 *
 * @param {string} number - The phone number to generate the link for
 * @param {string} [message] - Optional message to include in the link
 * @return {string} The generated WhatsApp link
 */
export const generateWhatsAppLink = (
  number: string,
  message?: string
): string =>
  new URL(
    `https://api.whatsapp.com/send?phone=${number.replace(/[\D+]/g, '')}${message ? `&text=${message}` : ''}`
  ).toString()

/**
 * Creates a resume from the given text by truncating it to a certain length and adding ellipsis if needed.
 *
 * @param {string} text - The input text to create a resume from.
 * @param {number} maxLength - The maximum length the resume should be truncated to.
 * @return {string} The truncated resume text.
 */
export function createResume(text: string, maxLength: number = 150): string {
  if (text.length > maxLength) {
    let resume = text.substring(0, maxLength)
    resume = resume.substring(
      0,
      Math.min(resume.length, resume.lastIndexOf(' '))
    )
    resume += '...'

    return resume
  } else {
    return text
  }
}
