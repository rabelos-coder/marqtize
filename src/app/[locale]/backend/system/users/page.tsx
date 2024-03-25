import { getTranslations } from 'next-intl/server'
import { Col, Container, Row } from 'reactstrap'

import { Breadcrumbs } from '@/components/backend/Breadcrumbs'
import { AclGuard } from '@/components/backend/Guards/AclGuard'
import { concatTitle } from '@/utils/helpers'
import { UsersList } from '@/views/backend/system/users/UsersList'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('users'))

  return {
    title,
  }
}

export default async function UsersPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale })

  const title = t('users')

  return (
    <AclGuard acl={{ action: 'Read', subject: 'User' }}>
      <div className="page-body">
        <Breadcrumbs title={title} pageTitle={title} />
        <Container fluid>
          <Row>
            <Col sm={12}>
              <UsersList />
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
