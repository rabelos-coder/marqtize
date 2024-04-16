import dynamic from 'next/dynamic'

const ColorComponent = dynamic(() => import('./ColorComponent'))
const LayoutType = dynamic(() => import('./LayoutType'))
const MixLayoutComponent = dynamic(() => import('./MixLayoutComponent'))
const SidebarIconType = dynamic(() => import('./SidebarIconType'))
const SidebarType = dynamic(() => import('./SidebarType'))

const SidebarCustomizer = () => {
  return (
    <>
      <LayoutType />
      <SidebarType />
      <SidebarIconType />
      <ColorComponent />
      <MixLayoutComponent />
    </>
  )
}

export default SidebarCustomizer
