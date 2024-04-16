'use client'

import Image from 'next/image'
import { useTranslations } from 'next-intl'
import { Button, Col, Container } from 'reactstrap'

import { useRouter } from '@/navigation'
import { CommonErrorPageProps } from '@/types/common'

const ErrorPage = ({
  title,
  description,
  titleClassName,
  color,
}: CommonErrorPageProps) => {
  const t = useTranslations()
  const router = useRouter()

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
            <h2 className={`headline ${titleClassName}`}>{title}</h2>
          </div>
          <Col md={8} className="offset-md-2">
            <div className="sub-content">{description}</div>
          </Col>
          <div>
            <Button
              color={color}
              size="lg"
              className={`text-uppercase`}
              onClick={() => router.back()}
            >
              {t('backToPreviousPage')}
            </Button>
          </div>
        </Container>
      </div>
    </div>
  )
}

export default ErrorPage
