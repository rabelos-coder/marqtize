import dynamic from 'next/dynamic'

const ColorComponent = dynamic(() => import('./ColorComponent'), { ssr: false })
const LayoutType = dynamic(() => import('./LayoutType'), { ssr: false })
const MixLayoutComponent = dynamic(() => import('./MixLayoutComponent'), {
  ssr: false,
})
const SidebarIconType = dynamic(() => import('./SidebarIconType'), {
  ssr: false,
})
const SidebarType = dynamic(() => import('./SidebarType'), { ssr: false })

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
