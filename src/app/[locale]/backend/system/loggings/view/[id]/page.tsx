import { getTranslations } from 'next-intl/server'
import { Card, Col, Container, Row } from 'reactstrap'

import { Breadcrumbs } from '@/components/backend/Breadcrumbs'
import { AclGuard } from '@/components/backend/Guards/AclGuard'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import { concatTitle } from '@/utils/helpers'
import { LoggingView } from '@/views/backend/system/loggings/LoggingView'

import style from './page.module.scss'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('viewName', { name: t('logging') }))

  return {
    title,
  }
}

export default async function UserViewPage({ params: { locale, id } }: any) {
  const t = await getTranslations({ locale })
  const title = t('viewName', { name: t('logging') })
  const pageTitle = t('logging')

  return (
    <AclGuard acl={{ action: 'Read', subject: 'User' }}>
      <div className="page-body">
        <Breadcrumbs
          title={title}
          pageTitle={pageTitle}
          subParent={t('loggings')}
        />
        <Container fluid>
          <Row>
            <Col sm="12">
              <Card className="height-equal">
                <CommonCardHeading
                  smallHeading={title}
                  span={t('viewFormAboutName', {
                    gender: 'male',
                    name: t('logging').toLowerCase(),
                  })}
                />
                <LoggingView id={id} style={style} />
              </Card>
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
