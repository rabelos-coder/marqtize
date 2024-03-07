import {
  HiHome,
  HiLockClosed,
  HiOutlineHome,
  HiOutlineLockClosed,
  HiOutlineUser,
  HiOutlineUsers,
  HiUser,
  HiUsers,
} from "react-icons/hi2";

import { SidebarMenuType } from "@/types/layout";

export const MenuListData: SidebarMenuType[] = [
  {
    title: "general",
    menuContent: "Dashboards",
    items: [
      {
        id: 1,
        title: "home",
        path: "backend",
        iconStroke: <HiOutlineHome className="stroke-icon" />,
        iconFill: <HiHome className="fill-icon" />,
        active: false,
        type: "link",
      },
    ],
  },
  {
    title: "authentication",
    menuContent: "Users,Roles,Claims",
    claims: ["User:Read", "Role:Read", "Claim:Read"],
    items: [
      {
        title: "users",
        id: 1,
        iconStroke: <HiOutlineUser className="stroke-icon" />,
        iconFill: <HiUser className="fill-icon" />,
        pathSlice: "users",
        type: "sub",
        active: false,
        claims: ["User:Read", "User:Create"],
        children: [
          {
            path: "backend/users/create",
            title: "createName",
            nameArgument: "user",
            type: "link",
            claims: ["User:Create"],
          },
          {
            path: "backend/users",
            title: "listName",
            nameArgument: "users",
            type: "link",
            claims: ["User:Read"],
          },
        ],
      },
      {
        title: "groups",
        id: 2,
        iconStroke: <HiOutlineUsers className="stroke-icon" />,
        iconFill: <HiUsers className="fill-icon" />,
        pathSlice: "groups",
        type: "sub",
        active: false,
        claims: ["Role:Read", "Role:Create"],
        children: [
          {
            path: "backend/groups/create",
            title: "createName",
            nameArgument: "group",
            type: "link",
          },
          {
            path: "backend/groups",
            title: "listName",
            nameArgument: "groups",
            type: "link",
          },
        ],
      },
      {
        title: "permissions",
        path: "backend/permissions",
        id: 3,
        claims: ["Claim:Read"],
        iconStroke: <HiOutlineLockClosed className="stroke-icon" />,
        iconFill: <HiLockClosed className="fill-icon" />,
        pathSlice: "permissions",
        type: "link",
      },
    ],
  },
];
