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
    claims: ['User:Read', 'Role:Read', 'Claim:Read'],
    items: [
      {
        id: uuidv4(),
        title: 'users',
        iconStroke: <HiOutlineUser className="stroke-icon" />,
        iconFill: <HiUser className="fill-icon" />,
        pathSlice: 'users',
        type: 'sub',
        active: false,
        claims: ['User:Read', 'User:Create'],
        children: [
          {
            path: 'backend/system/users/create',
            title: 'createName',
            nameArgument: 'user',
            type: 'link',
            claims: ['User:Create'],
          },
          {
            path: 'backend/system/users',
            title: 'listName',
            nameArgument: 'users',
            type: 'link',
            claims: ['User:Read'],
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
        claims: ['Role:Read', 'Role:Create'],
        children: [
          {
            path: 'backend/system/groups/create',
            title: 'createName',
            nameArgument: 'group',
            type: 'link',
          },
          {
            path: 'backend/system/groups',
            title: 'listName',
            nameArgument: 'groups',
            type: 'link',
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
