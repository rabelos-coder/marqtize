import { useTranslations } from 'next-intl'

import ErrorPage from '@/components/common/ErrorPage'

const NotAuthorized = () => {
  const t = useTranslations()

  return (
    <ErrorPage
      title={403}
      description={t('forbiddenInfo')}
      titleClassName="font-warning"
      color="warning-gradient"
    />
  )
}

export default NotAuthorized
