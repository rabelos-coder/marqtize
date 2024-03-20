import React from 'react'
import { TabContent, TabPane } from 'reactstrap'

import CheckLayout from '../Tabs/CheckLayout'
import SidebarCustomizer from '../Tabs/SidebarCustomizer'
import TabHeader from './TabHeader'

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
