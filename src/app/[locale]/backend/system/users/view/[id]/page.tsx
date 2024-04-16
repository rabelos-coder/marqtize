import { getTranslations } from 'next-intl/server'
import { Card, Col, Container, Row } from 'reactstrap'

import Breadcrumbs from '@/components/backend/Breadcrumbs'
import AclGuard from '@/components/backend/Guards/AclGuard'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import { ChildrenWithParamsProps, PageParamsProps } from '@/types/common'
import { concatTitle } from '@/utils/helpers'
import { UsersView } from '@/views/backend/system/users/UsersView'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('viewName', { name: t('user') }))

  return {
    title,
  }
}

export default async function UserViewPage({
  params: { locale, id },
}: ChildrenWithParamsProps) {
  const t = await getTranslations({ locale })
  const title = t('viewName', { name: t('user') })
  const pageTitle = t('user')

  return (
    <AclGuard acl={{ action: 'Read', subject: 'User' }}>
      <div className="page-body">
        <Breadcrumbs
          title={title}
          pageTitle={pageTitle}
          subParent={t('users')}
        />
        <Container fluid>
          <Row>
            <Col sm="12">
              <Card className="height-equal">
                <CommonCardHeading
                  smallHeading={title}
                  span={t('viewFormAboutName', {
                    gender: 'male',
                    name: t('user').toLowerCase(),
                  })}
                />
                <UsersView id={`${id}`} />
              </Card>
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
