import { getTranslations } from 'next-intl/server'
import { Card, Col, Container, Row } from 'reactstrap'

import Breadcrumbs from '@/components/backend/Breadcrumbs'
import AclGuard from '@/components/backend/Guards/AclGuard'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import { concatTitle } from '@/utils/helpers'
import { GroupsView } from '@/views/backend/system/groups/GroupsView'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('viewName', { name: t('role') }))

  return {
    title,
  }
}

export default async function RoleViewPage({ params: { locale, id } }: any) {
  const t = await getTranslations({ locale })
  const title = t('viewName', { name: t('role') })
  const pageTitle = t('role')

  return (
    <AclGuard acl={{ action: 'Read', subject: 'Role' }}>
      <div className="page-body">
        <Breadcrumbs
          title={title}
          pageTitle={pageTitle}
          subParent={t('roles')}
        />
        <Container fluid>
          <Row>
            <Col sm="12">
              <Card className="height-equal">
                <CommonCardHeading
                  smallHeading={title}
                  span={t('viewFormAboutName', {
                    gender: 'male',
                    name: t('role').toLowerCase(),
                  })}
                />
                <GroupsView id={`${id}`} />
              </Card>
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
