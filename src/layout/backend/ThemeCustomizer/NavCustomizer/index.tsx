import { useTranslations } from 'next-intl'
import React from 'react'
import { Nav, NavItem, NavLink } from 'reactstrap'

type NavCustomizerType = {
  callbackNav: (test: string, open: boolean) => void
  selected: string
}

const NavCustomizer = ({ callbackNav, selected }: NavCustomizerType) => {
  const t = useTranslations()

  return (
    <Nav className="flex-column nac-pills">
      <NavItem>
        <NavLink
          className={selected === 'check-layout' ? 'active' : ''}
          onClick={() => callbackNav('check-layout', true)}
        >
          <div className="settings">
            <i className="icon-paint-bucket"></i>
          </div>
          <span>{t('checkLayouts')}</span>
        </NavLink>
      </NavItem>
      <NavItem>
        <NavLink
          className={selected === 'sidebar-type' ? 'active' : ''}
          onClick={() => callbackNav('sidebar-type', true)}
        >
          <div className="settings">
            <i className="icon-settings"></i>
          </div>
          <span>{t('quickOptions')}</span>
        </NavLink>
      </NavItem>
    </Nav>
  )
}

export default NavCustomizer
