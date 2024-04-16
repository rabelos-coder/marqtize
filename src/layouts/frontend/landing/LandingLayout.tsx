'use client'

import '../../../app/assets/scss/landing.scss'

import dynamic from 'next/dynamic'
import { useEffect } from 'react'

const TapTop = dynamic(() => import('@/components/common/TapTop'))
const Navbar = dynamic(() => import('./Navbar/index'))
const Footer = dynamic(() => import('./Footer/index'))
const DefaultLayout = dynamic(() => import('../default/DefaultLayout'))

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
const LandingLayout = ({ navbarExpanded, children }: LandingProps) => {
  useEffect(() => {
    if (typeof window !== 'undefined') window.scrollTo({ top: 0, left: 0 })
  }, [])

  return (
    <DefaultLayout>
      <div id="layoutDefault">
        <div id="layoutDefault_content">
          <Navbar navbarExpanded={navbarExpanded} />
          {children}
        </div>
        <Footer />
        <TapTop />
      </div>
    </DefaultLayout>
  )
}

export default LandingLayout
