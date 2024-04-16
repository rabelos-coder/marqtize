import dynamic from 'next/dynamic'
import Image from 'next/image'

import { useCustomizer, useLayout } from '@/hooks'
import { Link } from '@/navigation'

const SidebarLogo = dynamic(() => import('./SidebarLogo'))
const SidebarMenu = dynamic(() => import('./SidebarMenu'))

const SideBar = () => {
  const { sidebarIconType } = useCustomizer()
  const { sideBarToggle } = useLayout()

  return (
    <div
      className={`sidebar-wrapper ${sideBarToggle ? 'close_icon' : ''}`}
      sidebar-layout={sidebarIconType}
    >
      <div>
        <SidebarLogo />
        <div className="logo-icon-wrapper">
          <Link href={'/backend'}>
            <Image
              width={35}
              height={35}
              className="img-fluid"
              src={`/assets/images/logo/marqtize_logo_solo_312x312.png`}
              alt=""
            />
          </Link>
        </div>
        <SidebarMenu />
      </div>
    </div>
  )
}

export default SideBar
