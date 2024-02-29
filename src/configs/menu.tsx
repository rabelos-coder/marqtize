import { AiFillDashboard, AiOutlineDashboard } from "react-icons/ai";
import {
  HiLockClosed,
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
        title: "dashboard",
        path: "backend",
        iconStroke: <AiOutlineDashboard className="stroke-icon" />,
        iconFill: <AiFillDashboard className="fill-icon" />,
        active: false,
        type: "link",
      },
    ],
  },
  {
    title: "authentication",
    menuContent: "Users,Roles,Claims",
    items: [
      {
        title: "users",
        id: 1,
        iconStroke: <HiOutlineUser className="stroke-icon" />,
        iconFill: <HiUser className="fill-icon" />,
        pathSlice: "users",
        type: "sub",
        active: false,
        children: [
          {
            path: "backend/users/create",
            title: "createName",
            nameArgument: "user",
            type: "link",
          },
          {
            path: "backend/users",
            title: "listName",
            nameArgument: "users",
            type: "link",
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
        iconStroke: <HiOutlineLockClosed className="stroke-icon" />,
        iconFill: <HiLockClosed className="fill-icon" />,
        pathSlice: "permissions",
        type: "link",
      },
    ],
  },
];
