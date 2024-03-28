import {
  HiHome,
  HiLockClosed,
  HiOutlineHome,
  HiOutlineLockClosed,
  HiOutlineUser,
  HiOutlineUsers,
  HiUser,
  HiUsers,
} from 'react-icons/hi2'
import { v4 as uuidv4 } from 'uuid'

import { SidebarMenuType } from '@/types/layout'

export const MenuListData: SidebarMenuType[] = [
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
