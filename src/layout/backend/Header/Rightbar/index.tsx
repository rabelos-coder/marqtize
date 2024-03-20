import React from 'react'
import { Col } from 'reactstrap'

import { THEME_LOCALE_SWITCHER_ENABLED } from '@/environment'

import { Language } from './Languages/index'
import { MoonLight } from './MoonLight'
import Profile from './Profile'

export const Rightbar = () => {
  return (
    <Col
      xxl={7}
      xl={6}
      md={7}
      xs={8}
      className="nav-right pull-right right-header p-0 ms-auto"
    >
      <ul className="nav-menus flex-row">
        {THEME_LOCALE_SWITCHER_ENABLED && <Language />}
        {/* <SearchBar /> */}
        <MoonLight />
        <Profile />
      </ul>
    </Col>
  )
}
