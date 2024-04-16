import { getTranslations } from 'next-intl/server'
import { Col, Container, Row } from 'reactstrap'

import Breadcrumbs from '@/components/backend/Breadcrumbs'
import AclGuard from '@/components/backend/Guards/AclGuard'
import { concatTitle } from '@/utils/helpers'
import { LoggingList } from '@/views/backend/system/loggings/LoggingList'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('logging'))

  return {
    title,
  }
}

export default async function LoggingsPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale })

  const title = t('loggings')

  return (
    <AclGuard acl={{ action: 'Read', subject: 'Logging' }}>
      <div className="page-body">
        <Breadcrumbs title={title} pageTitle={title} />
        <Container fluid>
          <Row>
            <Col sm={12}>
              <LoggingList />
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
