'use client'

import * as Sentry from '@sentry/nextjs'
import Image from 'next/image'
import { useTranslations } from 'next-intl'
import { useEffect } from 'react'
import { Button, Col, Container } from 'reactstrap'

import { IS_DEVELOPMENT } from '@/environment'

export default function ErrorPage({
  error,
  reset,
}: {
  error: Error & { digest?: string }
  reset: () => void
}) {
  const t = useTranslations()

  useEffect(() => {
    !IS_DEVELOPMENT ? Sentry.captureException(error) : console.error(error)
  }, [error])

  return (
    <div className="page-wrapper compact-wrapper" id="pageWrapper">
      <div className="error-wrapper">
        <Container>
          <Image
            width={100}
            height={100}
            className="img-100"
            src={`/assets/images/other-images/sad.png`}
            alt="Error"
          />
          <div className="error-heading">
            <h2 className={`headline font-primary`}>500</h2>
          </div>
          <Col md={8} className="offset-md-2">
            <div className="sub-content">{t('internalServerErrorInfo')}</div>
          </Col>
          <div>
            <Button
              color="primary-gradient"
              size="lg"
              className={`text-uppercase`}
              onClick={reset}
            >
              {t('reload')}
            </Button>
          </div>
        </Container>
      </div>
    </div>
  )
}
