import { getTranslations } from 'next-intl/server'
import { Card, Col, Container, Row } from 'reactstrap'

import Breadcrumbs from '@/components/backend/Breadcrumbs'
import AclGuard from '@/components/backend/Guards/AclGuard'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import { PageParamsProps } from '@/types/common'
import { concatTitle } from '@/utils/helpers'
import { GroupsForm } from '@/views/backend/system/groups/GroupsForm'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('createName', { name: t('role') }))

  return {
    title,
  }
}

export default async function RoleCreatePage({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = t('createName', { name: t('role') })
  const pageTitle = t('role')

  return (
    <AclGuard acl={{ action: 'Create', subject: 'Role' }}>
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
                  span={t('createFormAboutName', {
                    gender: 'male',
                    name: t('role').toLowerCase(),
                  })}
                />
                <GroupsForm mode="create" />
              </Card>
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
