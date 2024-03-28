import { useTranslations } from 'next-intl'
import { Fragment } from 'react'

import SvgIcon from '@/components/common/SvgIcon'
import { useAbility, useAppDispatch, useCustomizer, useLayout } from '@/hooks'
import { useRouter } from '@/navigation'
import { setPinnedMenu } from '@/store/slices/themeSlice'
import { SidebarItemType } from '@/types/layout'

type MenuListType = {
  menuItems: SidebarItemType[]
  handleActive: (title: string, level: number) => void
  active: string
  setActiveLink: Function
  setActive: Function
  activeLink: string | undefined
  level: number
  className?: string
}

export const MenuList = ({
  setActive,
  handleActive,
  active,
  menuItems,
  level,
  activeLink,
  setActiveLink,
}: MenuListType) => {
  const { pinnedMenu } = useLayout()
  const dispatch = useAppDispatch()
  const handlePined = (value: string | undefined) => {
    if (!pinnedMenu?.includes(value || '')) {
      dispatch(setPinnedMenu([...pinnedMenu, value ?? '']))
    } else {
      const filterMenu = pinnedMenu?.filter((item) => item !== value)
      dispatch(setPinnedMenu(filterMenu))
    }
  }
  const router = useRouter()
  const { layoutName, sidebarIconType } = useCustomizer()
  const t = useTranslations()
  const ability = useAbility()

  const canAny = (claims: string[]) => {
    if (!claims?.length) {
      return true
    } else if (claims?.length) {
      return claims.some((claim) => {
        const [subject, action] = claim.split(':')

        return ability?.can(action, subject)
      })
    }

    return false
  }

  return (
    <>
      {menuItems.map((item, i) =>
        canAny(item?.claims ?? []) ? (
          <li
            key={i}
            className={`${pinnedMenu.includes(item.title || '') ? 'pined' : ''} ${
              level == 0 ? 'sidebar-list' : ''
            }  `}
          >
            {level === 0 && (
              <i
                className="fa fa-thumb-tack"
                onClick={() => handlePined(item.title)}
              ></i>
            )}
            <a
              style={{ cursor: 'pointer' }}
              className={
                level === 0
                  ? `sidebar-link sidebar-title  ${
                      (item.pathSlice && active.includes(item.pathSlice)) ||
                      activeLink ==
                        item.path?.split('/')[item.path.split('/').length - 1]
                        ? 'active'
                        : ''
                    }`
                  : `text-decoration-none ${
                      activeLink ==
                      item.path?.split('/')[item.path.split('/').length - 1]
                        ? 'active'
                        : ''
                    }`
              }
              onClick={() => {
                if (item.type == 'sub') {
                  handleActive(item.pathSlice ? item.pathSlice : '', level)
                } else {
                  if (level == 0) {
                    setActive('')
                  }
                  setActiveLink(
                    item.path?.split('/')[item.path.split('/').length - 1]
                  )
                  router.push(
                    layoutName
                      ? item.path + `?layout=${layoutName.toLowerCase()}`
                      : `/${item.path}`
                  )
                }
              }}
            >
              {typeof item.icon === 'string' ? (
                <>
                  {item.icon && (
                    <SvgIcon
                      className="stroke-icon"
                      iconId={`stroke-${item.icon}`}
                    />
                  )}
                  {item.icon && (
                    <SvgIcon
                      className="fill-icon"
                      iconId={`fill-${item.icon}`}
                    />
                  )}
                </>
              ) : (
                <>
                  {item.icon
                    ? item.icon
                    : sidebarIconType === 'stroke-svg'
                      ? item.iconStroke
                      : item.iconFill}
                </>
              )}
              <span>
                {item.title?.endsWith('Name')
                  ? t(`${item.title}`, { name: t(`${item.nameArgument}`) })
                  : t(`${item.title}`)}
              </span>
              {item.badge ? (
                <label className={item.badge}>{item.badgeTxt}</label>
              ) : (
                ''
              )}
              {item.children && (
                <div className="according-menu">
                  {item.pathSlice && active.includes(item.pathSlice) ? (
                    <i className="fa fa-angle-down" />
                  ) : (
                    <i className="fa fa-angle-right" />
                  )}
                </div>
              )}
            </a>
            {item.children && (
              <ul
                className={` ${
                  level >= 1
                    ? 'nav-sub-childmenu submenu-content'
                    : 'sidebar-submenu list-group'
                }`}
                style={
                  item.pathSlice && active.includes(item.pathSlice)
                    ? {
                        opacity: '1',
                        transition: 'opacity 500ms ease-in 0s',
                        display: 'block',
                      }
                    : { display: 'none' }
                }
              >
                <MenuList
                  setActive={setActive}
                  menuItems={item.children}
                  handleActive={handleActive}
                  active={active}
                  level={level + 1}
                  activeLink={activeLink}
                  setActiveLink={setActiveLink}
                />
              </ul>
            )}
          </li>
        ) : (
          <Fragment key={i} />
        )
      )}
    </>
  )
}
