import { useState } from 'react'

import { CountryClassName } from '@/types/language'

import { LanguageList } from './LanguageList'

export const Language = () => {
  const [dropdownShow, setDropdownShow] = useState(false)
  const [selected, setSelected] = useState('pt-br')

  const LanguageSelection = (dropdownShow: boolean) => {
    if (selected) {
      setDropdownShow(!dropdownShow)
    } else {
      setDropdownShow(!dropdownShow)
    }
  }

  return (
    <li className="language-nav">
      <div className={`translate_wrapper ${dropdownShow ? 'active' : ''}`}>
        <div className="current_lang">
          <div className="lang" onClick={() => LanguageSelection(dropdownShow)}>
            <i
              className={`flag-icon flag-icon-${CountryClassName[selected]}`}
            ></i>
            <span className="lang-txt">{CountryClassName[selected]}</span>
          </div>
        </div>
        <LanguageList selected={selected} setSelected={setSelected} />
      </div>
    </li>
  )
}
