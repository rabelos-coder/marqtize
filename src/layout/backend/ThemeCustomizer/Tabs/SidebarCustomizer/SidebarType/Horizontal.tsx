import React from 'react'

import CommonUL from '../CommonUL'

type PropsType = {
  handleSidebarType: (data: string) => void
  layout?: string
}

const Horizontal = ({ handleSidebarType, layout }: PropsType) => {
  return (
    <li
      data-attr="compact-sidebar"
      className={`border-0 ${layout === 'compact-wrapper' ? 'active' : ''}`}
      onClick={() => handleSidebarType('compact-wrapper')}
    >
      <div className="header bg-light">
        <CommonUL />
      </div>
      <div className="body">
        <ul className="flex-row">
          <li className="bg-dark sidebar compact"></li>
          <li className="bg-light body"></li>
        </ul>
      </div>
    </li>
  )
}

export default Horizontal