import { getTranslations } from 'next-intl/server'
import { Col, Container, Row } from 'reactstrap'

import RatioImage from '@/components/backend/RatioImage'
import { PageParamsProps } from '@/types/common'
import { concatTitle } from '@/utils/helpers'
import { ForgotPasswordForm } from '@/views/auth/ForgotPasswordForm'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('forgotPassword'))

  return {
    title,
  }
}

export default async function ForgotPasswordPage() {
  return (
    <Container fluid>
      <Row>
        <Col xl={7} className="b-center bg-size">
          <RatioImage
            className="bg-img-cover bg-center img-fluid w-100"
            src={`/assets/images/login/1.jpg`}
            alt=""
          />
        </Col>
        <Col xl={5} className="p-0">
          <ForgotPasswordForm alignLogo="text-start" />
        </Col>
      </Row>
    </Container>
  )
}
