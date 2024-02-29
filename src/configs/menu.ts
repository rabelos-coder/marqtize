import { SidebarMenuType } from "@/types/layout";

export const MenuList: SidebarMenuType[] = [
  {
    title: "general",
    menuContent: "Dashboards",
    items: [
      {
        id: 1,
        title: "dashboard",
        path: "backend",
        icon: "home",
        active: false,
        type: "link",
      },
    ],
  },
];
