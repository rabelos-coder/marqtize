import { getTranslations } from 'next-intl/server'
import { Card, CardBody, Col, Container, Row } from 'reactstrap'

import { Breadcrumbs } from '@/components/backend/Breadcrumbs'
import { AclGuard } from '@/components/backend/Guards/AclGuard'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import { concatTitle } from '@/utils/helpers'
import { UsersList } from '@/views/backend/users/UsersList'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('users'))

  return {
    title,
  }
}

export default async function UsersPage({ params: { locale, page } }: any) {
  const t = await getTranslations({ locale })

  const title = t('users')
  const pageTitle = t('listName', { name: t('users') })
  const pageDescription = t('seeInformationAboutName', {
    gender: 'male',
    name: t('users').toLowerCase(),
  })

  return (
    <AclGuard acl={{ action: 'Read', subject: 'User' }}>
      <div className="page-body">
        <Breadcrumbs title={title} pageTitle={title} />
        <Container fluid>
          <Row>
            <Col sm="12">
              <Card>
                <CommonCardHeading
                  smallHeading={pageTitle}
                  span={pageDescription}
                />
                <CardBody>
                  <UsersList page={parseInt(`${page}`)} />
                </CardBody>
              </Card>
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
