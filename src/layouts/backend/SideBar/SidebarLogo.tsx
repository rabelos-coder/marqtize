import { useLayout } from "@/hooks";
import Image from "next/image";
import { Link } from "@/navigation";
import FeatherIconCom from "@/components/common/Icons/FeatherIconCom";

export const SideBarLogo = () => {
  const { setSideBarToggle, sideBarToggle } = useLayout();

  return (
    <div className="logo-wrapper">
      <Link href={"/dashboard/default"}>
        <Image
          className="img-fluid for-light"
          src={"/assets/images/logo/logo.png"}
          alt="icon"
          width={121}
          height={100}
        />
        <Image
          className="img-fluid for-dark"
          src={"/assets/images/logo/logo_dark.png"}
          alt="icon"
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
