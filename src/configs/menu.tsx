import { HiOutlineGlobeAlt } from '@react-icons/all-files/hi/HiOutlineGlobeAlt'
import { HiOutlineHome } from '@react-icons/all-files/hi/HiOutlineHome'
import { HiOutlineLockClosed } from '@react-icons/all-files/hi/HiOutlineLockClosed'
import { HiOutlineUser } from '@react-icons/all-files/hi/HiOutlineUser'
import { HiOutlineUsers } from '@react-icons/all-files/hi/HiOutlineUsers'
import { HiUser } from '@react-icons/all-files/hi/HiUser'
import { HiUsers } from '@react-icons/all-files/hi/HiUsers'
import { HiOutlineOfficeBuilding } from '@react-icons/all-files/hi/HiOutlineOfficeBuilding'
import { HiOfficeBuilding } from '@react-icons/all-files/hi/HiOfficeBuilding'
import { HiGlobeAlt } from '@react-icons/all-files/hi/HiGlobeAlt'
import { HiHome } from '@react-icons/all-files/hi/HiHome'
import { HiLockClosed } from '@react-icons/all-files/hi/HiLockClosed'

import { v4 as uuidv4 } from 'uuid'

import { SidebarMenuType } from '@/types/layout'

const MenuListData: SidebarMenuType[] = [
  {
    title: 'general',
    menuContent: 'Dashboards,Dashboard',
    items: [
      {
        id: uuidv4(),
        title: 'home',
        path: 'backend',
        iconStroke: <HiOutlineHome className="stroke-icon" />,
        iconFill: <HiHome className="fill-icon" />,
        active: false,
        type: 'link',
      },
    ],
  },
  {
    title: 'management',
    menuContent: 'Accounts',
    claims: ['Account:Manage', 'Account:Read'],
    items: [
      {
        id: uuidv4(),
        title: 'accounts',
        iconStroke: <HiOutlineOfficeBuilding className="stroke-icon" />,
        iconFill: <HiOfficeBuilding className="fill-icon" />,
        pathSlice: 'accounts',
        type: 'sub',
        active: false,
        claims: ['Account:Manage', 'Account:Read', 'Account:Create'],
        children: [
          {
            path: 'backend/accounts/create',
            title: 'createName',
            nameArgument: 'account',
            type: 'link',
            claims: ['Account:Manage', 'Account:Create'],
          },
          {
            path: 'backend/accounts',
            title: 'listName',
            nameArgument: 'accounts',
            type: 'link',
            claims: ['Account:Manage', 'Account:Read'],
          },
        ],
      },
    ],
  },
  {
    title: 'authentication',
    menuContent: 'Users,Roles,Claims',
    claims: [
      'User:Manage',
      'User:Read',
      'Role:Manage',
      'Role:Read',
      'Claim:Read',
    ],
    items: [
      {
        id: uuidv4(),
        title: 'users',
        iconStroke: <HiOutlineUser className="stroke-icon" />,
        iconFill: <HiUser className="fill-icon" />,
        pathSlice: 'users',
        type: 'sub',
        active: false,
        claims: ['User:Manage', 'User:Read', 'User:Create'],
        children: [
          {
            path: 'backend/system/users/create',
            title: 'createName',
            nameArgument: 'user',
            type: 'link',
            claims: ['User:Manage', 'User:Create'],
          },
          {
            path: 'backend/system/users',
            title: 'listName',
            nameArgument: 'users',
            type: 'link',
            claims: ['User:Manage', 'User:Read'],
          },
        ],
      },
      {
        id: uuidv4(),
        title: 'groups',
        iconStroke: <HiOutlineUsers className="stroke-icon" />,
        iconFill: <HiUsers className="fill-icon" />,
        pathSlice: 'groups',
        type: 'sub',
        active: false,
        claims: ['Role:Manage', 'Role:Read', 'Role:Create'],
        children: [
          {
            path: 'backend/system/groups/create',
            title: 'createName',
            nameArgument: 'group',
            type: 'link',
            claims: ['Role:Manage', 'Role:Create'],
          },
          {
            path: 'backend/system/groups',
            title: 'listName',
            nameArgument: 'groups',
            type: 'link',
            claims: ['Role:Manage', 'Role:Read'],
          },
        ],
      },
    ],
  },
  {
    title: 'system',
    menuContent: 'Log,Logging,Claims',
    claims: ['Logging:Manage', 'Logging:Read', 'Claim:Read'],
    items: [
      {
        id: uuidv4(),
        title: 'loggings',
        path: 'backend/system/loggings',
        claims: ['Logging:Manage', 'Logging:Read'],
        iconStroke: <HiOutlineGlobeAlt className="stroke-icon" />,
        iconFill: <HiGlobeAlt className="fill-icon" />,
        pathSlice: 'logging',
        type: 'link',
      },
      {
        id: uuidv4(),
        title: 'permissions',
        path: 'backend/system/permissions',
        claims: ['Claim:Read'],
        iconStroke: <HiOutlineLockClosed className="stroke-icon" />,
        iconFill: <HiLockClosed className="fill-icon" />,
        pathSlice: 'permissions',
        type: 'link',
      },
    ],
  },
]

export default MenuListData
