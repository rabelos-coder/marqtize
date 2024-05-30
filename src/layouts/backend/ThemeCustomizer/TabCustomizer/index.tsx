import dynamic from 'next/dynamic'
import { TabContent, TabPane } from 'reactstrap'

const CheckLayout = dynamic(() => import('../Tabs/CheckLayout'), { ssr: false })
const SidebarCustomizer = dynamic(() => import('../Tabs/SidebarCustomizer'), {
  ssr: false,
})
const TabHeader = dynamic(() => import('./TabHeader'), { ssr: false })

type TabCustomizerType = {
  callbackNav: (test: string, open: boolean) => void
  selected: string
}

const TabCustomizer = ({ callbackNav, selected }: TabCustomizerType) => {
  return (
    <TabContent activeTab={selected}>
      <TabHeader callbackNav={callbackNav} />
      <div className="customizer-body custom-scrollbar tab-content">
        <TabPane tabId="check-layout">
          <CheckLayout />
        </TabPane>
        <TabPane tabId="sidebar-type">
          <SidebarCustomizer />
        </TabPane>
      </div>
    </TabContent>
  )
}

export default TabCustomizer
