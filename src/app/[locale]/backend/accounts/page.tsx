import { getTranslations } from 'next-intl/server'
import { Col, Container, Row } from 'reactstrap'

import Breadcrumbs from '@/components/backend/Breadcrumbs'
import AclGuard from '@/components/backend/Guards/AclGuard'
import { concatTitle } from '@/utils/helpers'
import { AccountsList } from '@/views/backend/accounts/AccountsList'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('accounts'))

  return {
    title,
  }
}

export default async function AccountsPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale })

  const title = t('accounts')

  return (
    <AclGuard acl={{ action: 'Read', subject: 'Account' }}>
      <div className="page-body">
        <Breadcrumbs title={title} pageTitle={title} />
        <Container fluid>
          <Row>
            <Col sm={12}>
              <AccountsList />
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
