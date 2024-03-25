import { getTranslations } from 'next-intl/server'
import { Col, Container, Row } from 'reactstrap'

import { Breadcrumbs } from '@/components/backend/Breadcrumbs'
import { AclGuard } from '@/components/backend/Guards/AclGuard'
import { concatTitle } from '@/utils/helpers'
import { GroupsList } from '@/views/backend/system/groups/GroupsList'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('groups'))

  return {
    title,
  }
}

export default async function GroupsPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale })

  const title = t('groups')

  return (
    <AclGuard acl={{ action: 'Read', subject: 'Role' }}>
      <div className="page-body">
        <Breadcrumbs title={title} pageTitle={title} />
        <Container fluid>
          <Row>
            <Col sm={12}>
              <GroupsList />
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
