import Image from "next/image";

import FeatherIconCom from "@/components/common/Icons/FeatherIconCom";
import { useLayout } from "@/hooks";
import { Link } from "@/navigation";

export const SideBarLogo = () => {
  const { setSideBarToggle, sideBarToggle } = useLayout();

  return (
    <div className="logo-wrapper">
      <Link href={"/backend"}>
        <Image
          className="img-fluid for-light"
          src={"/assets/images/logo/logo.png"}
          alt=""
          width={121}
          height={100}
        />
        <Image
          className="img-fluid for-dark"
          src={"/assets/images/logo/logo_dark.png"}
          alt=""
          width={121}
          height={100}
        />
      </Link>
      <div
        className="back-btn"
        onClick={() => setSideBarToggle(!sideBarToggle)}
      >
        <i className="fa fa-angle-left" />
      </div>
      <div
        className="toggle-sidebar"
        onClick={() => setSideBarToggle(!sideBarToggle)}
      >
        <FeatherIconCom
          iconName={"Grid"}
          className="status_toggle middle sidebar-toggle"
        />
      </div>
    </div>
  );
};
