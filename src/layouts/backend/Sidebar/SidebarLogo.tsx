import Image from 'next/image'
import { FiGrid } from 'react-icons/fi'

import { useLayout } from '@/hooks'
import { Link } from '@/navigation'

const SidebarLogo = () => {
  const { setSideBarToggle, sideBarToggle } = useLayout()

  return (
    <div className="logo-wrapper">
      <Link href={'/backend'}>
        <Image
          className="img-fluid for-light"
          src={'/assets/images/logo/marqtize_logo.png'}
          alt=""
          width={75}
          height={37}
        />
        <Image
          className="img-fluid for-dark"
          src={'/assets/images/logo/marqtize_logo_dark.png'}
          alt=""
          width={75}
          height={37}
        />
      </Link>
      <div
        className="back-btn"
        onClick={() => setSideBarToggle(!sideBarToggle)}
      >
        <i className="fa fa-angle-left" />
      </div>
      <div
        className="toggle-sidebar"
        onClick={() => setSideBarToggle(!sideBarToggle)}
      >
        <FiGrid className="status_toggle middle sidebar-toggle" />
      </div>
    </div>
  )
}

export default SidebarLogo
