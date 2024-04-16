import { getTranslations } from 'next-intl/server'
import { Container, Row } from 'reactstrap'

import Breadcrumbs from '@/components/backend/Breadcrumbs'
import AclGuard from '@/components/backend/Guards/AclGuard'
import GoodsReturn from '@/components/backend/Home/GoodsReturn'
import GreetingCard from '@/components/backend/Home/GreetingCard'
import OrderProfit from '@/components/backend/Home/OrderProfit'
import SalePurchase from '@/components/backend/Home/SalePurchase'
import { APP_META_SLOGAN } from '@/environment'
import { ChildrenWithParamsProps } from '@/types/common'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata() {
  const title = concatTitle(APP_META_SLOGAN)

  return {
    title,
  }
}

export default async function BackendPage({
  params: { locale },
}: ChildrenWithParamsProps) {
  const t = await getTranslations({ locale })
  const title = t('home')

  return (
    <AclGuard>
      <div className="page-body">
        <Breadcrumbs pageTitle={title} />
        <Container fluid>
          <Row className="widget-grid">
            <GreetingCard />
            <SalePurchase />
            <GoodsReturn />
            <OrderProfit />
            {/* <OverBalance />
            <RecentOrders />
            <ActivityCard />
            <RecentSales />
            <TimeLineCard />
            <PreAccountCard />
            <TotalUserAndFollower />
            <PaperNote /> */}
          </Row>
        </Container>
      </div>
    </AclGuard>
  )
}
