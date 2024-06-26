'use client'

import dynamic from 'next/dynamic'
import { ReactNode, useEffect, useState } from 'react'

import MenuListData from '@/configs/menu'
import Theme from '@/configs/theme'
import { THEME_CUSTOMIZER_ENABLED } from '@/environment'
import { useAppSelector, useAuth, useCustomizer, useLayout } from '@/hooks'
import { ChildrenProps } from '@/types/common'
import { SearchableMenuType, SidebarItemType } from '@/types/layout'

const TapTop = dynamic(() => import('@/components/common/TapTop'), {
  ssr: false,
})
const Spinner = dynamic(() => import('@/components/common/Spinner'), {
  ssr: false,
})
const Header = dynamic(() => import('./Header'), { ssr: false })
const Footer = dynamic(() => import('./Footer'), { ssr: false })
const SideBar = dynamic(() => import('./Sidebar'), { ssr: false })
const ThemeCustomizer = dynamic(() => import('./ThemeCustomizer'), {
  ssr: false,
})

/**
 * Renders the authentication layout for the back-end application.
 *
 * The Authentication Layout contains the default components and scripts to shared across the back-end application.
 *
 * @param {ChildrenProps} children - The children components to be rendered.
 * @return {JSX.Element} The rendered authentication layout.
 */
const AuthLayout = ({ children }: ChildrenProps): JSX.Element => {
  const { layout, setLayout } = useCustomizer()
  const default_color = Theme.data.color.primary_color
  const secondary_color = Theme.data.color.secondary_color
  const [colorBackground1, setColorBackground1] = useState(default_color)
  const [colorBackground2, setColorBackground2] = useState(secondary_color)

  useEffect(() => {
    if (typeof document !== 'undefined') {
      document.documentElement.style.setProperty(
        '--theme-default',
        colorBackground1
      )
      document.documentElement.style.setProperty(
        '--theme-secondary',
        colorBackground2
      )
    }
    Theme.data.color.primary_color = colorBackground1
    Theme.data.color.secondary_color = colorBackground2
  }, [
    setColorBackground1,
    setColorBackground2,
    colorBackground1,
    colorBackground2,
  ])

  const {
    sideBarToggle,
    setSideBarToggle,
    setSearchableMenu,
    setBookmarkList,
  } = useLayout()

  const { user } = useAuth()
  const { loading, theme } = useAppSelector((state) => state.theme)

  const compactSidebar = () => {
    if (layout === 'compact-wrapper') {
      if (typeof window !== 'undefined' && window.innerWidth <= 1006) {
        setSideBarToggle(true)
      } else {
        setSideBarToggle(false)
      }
    } else if (layout === 'horizontal-wrapper') {
      if (typeof window !== 'undefined' && window.innerWidth <= 1006) {
        setSideBarToggle(true)
        setLayout('compact-wrapper')
      } else {
        setSideBarToggle(false)
        setLayout('horizontal-wrapper')
      }
    }
  }

  useEffect(() => {
    compactSidebar()
    if (typeof window !== 'undefined')
      window.addEventListener('resize', () => {
        compactSidebar()
      })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [layout])

  useEffect(() => {
    const suggestionArray: SearchableMenuType[] = []
    const bookmarkArray: SearchableMenuType[] = []
    let num = 0

    const getAllLink = (item: SidebarItemType, icon: ReactNode) => {
      if (item.children) {
        item.children.map((ele: SidebarItemType) => {
          getAllLink(ele, icon)
        })
      } else {
        num = num + 1
        suggestionArray.push({
          icon: icon,
          title: item.title ? item.title : '',
          path: item.path ? item.path : '',
          bookmarked: item.bookmark ? item.bookmark : false,
          id: num,
        })
        if (item.bookmark) {
          bookmarkArray.push({
            icon: icon,
            title: item.title ? item.title : '',
            path: item.path ? item.path : '',
            bookmarked: item.bookmark,
            id: num,
          })
        }
      }
    }

    MenuListData.forEach((item) => {
      item.items?.map((child) => {
        getAllLink(child, child.icon)
      })
    })
    setSearchableMenu(suggestionArray)
    setBookmarkList(bookmarkArray)

    if (theme === 'light') {
      document.body.classList.remove('dark-only')
      document.body.classList.add('light-only')
    } else {
      document.body.classList.remove('light-only')
      document.body.classList.add('dark-only')
    }

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [theme])

  return loading ? (
    <Spinner />
  ) : (
    <>
      <div
        className={`page-wrapper ${sideBarToggle ? 'compact-wrapper' : layout}`}
      >
        <Header />
        <div className="page-body-wrapper">
          <SideBar />
          {children}
          <Footer />
        </div>
      </div>
      {THEME_CUSTOMIZER_ENABLED && user?.isSuperAdmin && <ThemeCustomizer />}
      <TapTop />
    </>
  )
}

export default AuthLayout
