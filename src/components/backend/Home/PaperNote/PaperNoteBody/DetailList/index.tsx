'use client'

import { useTranslations } from 'next-intl'

const DetailList = () => {
  const t = useTranslations()

  return (
    <div className="d-flex align-items-center">
      <h5 className="mb-0 font-primary f-18 me-1">$239,098</h5>
      <span className="f-light f-w-500">{t('budget')}</span>
    </div>
  )
}

export default DetailList
