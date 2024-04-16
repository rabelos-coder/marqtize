import { getTranslations } from 'next-intl/server'
import { Col, Container, Row } from 'reactstrap'

import Breadcrumbs from '@/components/backend/Breadcrumbs'
import AclGuard from '@/components/backend/Guards/AclGuard'
import { concatTitle } from '@/utils/helpers'
import { PermissionsList } from '@/views/backend/system/permissions/PermissionsList'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('permissions'))

  return {
    title,
  }
}

export default async function PermissionsPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale })

  const title = t('permissions')

  return (
    <AclGuard acl={{ action: 'Read', subject: 'Claim' }}>
      <div className="page-body">
        <Breadcrumbs title={title} pageTitle={title} />
        <Container fluid>
          <Row>
            <Col sm={12}>
              <PermissionsList />
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
