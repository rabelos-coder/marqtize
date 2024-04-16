'use client'

import { useTranslations } from 'next-intl'
import { useEffect, useState } from 'react'
import { FaFacebookF, FaLinkedinIn, FaTwitter } from 'react-icons/fa'
import {
  FacebookShareButton,
  LinkedinShareButton,
  TwitterShareButton,
} from 'react-share'

const ShareButtons = () => {
  const t = useTranslations()
  const [url, setUrl] = useState('')

  useEffect(() => {
    if (typeof window !== 'undefined') setUrl(window.location.href)
  }, [])

  return (
    <div className="single-post-meta-links">
      <FacebookShareButton
        url={url}
        title={t('shareWithName', { name: 'Facebook' })}
      >
        <FaFacebookF width={18} height={18} />
      </FacebookShareButton>
      <TwitterShareButton
        url={url}
        title={t('shareWithName', { name: 'XTwitter' })}
      >
        <FaTwitter width={18} height={18} />
      </TwitterShareButton>
      <LinkedinShareButton
        url={url}
        title={t('shareWithName', { name: 'LinkedIn' })}
      >
        <FaLinkedinIn width={18} height={18} />
      </LinkedinShareButton>
    </div>
  )
}

export default ShareButtons
