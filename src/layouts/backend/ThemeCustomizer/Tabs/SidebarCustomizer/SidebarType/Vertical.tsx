import dynamic from 'next/dynamic'

const CommonUL = dynamic(() => import('../CommonUL'))

type PropsType = {
  handleSidebarType: (data: string) => void
  layout?: string
}

const Vertical = ({ handleSidebarType, layout }: PropsType) => {
  return (
    <li
      data-attr="normal-sidebar"
      className={`border-0 ${layout === 'horizontal-wrapper' ? 'active' : ''}`}
      onClick={() => handleSidebarType('horizontal-wrapper')}
    >
      <div className="header bg-light">
        <CommonUL />
      </div>
      <div className="body">
        <ul className="flex-row">
          <li className="bg-dark sidebar"></li>
          <li className="bg-light body"></li>
        </ul>
      </div>
    </li>
  )
}

export default Vertical