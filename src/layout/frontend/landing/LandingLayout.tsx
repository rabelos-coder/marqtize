'use client'

import '../../../app/assets/scss/landing.scss'

import { useEffect } from 'react'

import { TapTop } from '@/components/common/TapTop'

import { DefaultLayout } from '../default/DefaultLayout'
import { Footer } from './Footer'
import { NavBar } from './Navbar/index'

type LandingProps = {
  children: React.ReactNode
  navbarExpanded?: boolean
}

/**
 * Renders the landing layout for the application.
 *
 * The Landing Layout contains the default website components such as navbar and footer.
 *
 * @param {LandingProps} props - The props for the component.
 * @return {JSX.Element} The rendered landing layout.
 */
export const LandingLayout = ({ navbarExpanded, children }: LandingProps) => {
  useEffect(() => {
    if (typeof window !== 'undefined') window.scrollTo({ top: 0, left: 0 })
  }, [])

  return (
    <DefaultLayout>
      <div id="layoutDefault">
        <div id="layoutDefault_content">
          <NavBar navbarExpanded={navbarExpanded} />
          {children}
        </div>
        <Footer />
        <TapTop />
      </div>
    </DefaultLayout>
  )
}
