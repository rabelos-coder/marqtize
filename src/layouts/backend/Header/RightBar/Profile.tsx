import Image from "next/image";
import Link from "next/link";
import React from "react";
import { useRouter } from "@/navigation";
import FeatherIconCom from "@/components/common/Icons/FeatherIconCom";
import { ProfileListData } from "@/types/language";
import { signOut } from "next-auth/react";

const Profile = () => {
  const handleLogOut = async () => {
    await signOut({ callbackUrl: "/auth/login" });
  };

  return (
    <li className="profile-nav onhover-dropdown pe-0 py-0">
      <div className="media profile-media">
        <Image
          className="b-r-10"
          src="/assets/images/dashboard/profile.png"
          alt=""
          width={35}
          height={35}
        />
        <div className="media-body">
          <span>Emily Walter</span>
          <p className="mb-0 font-roboto">
            Admin <i className="middle fa fa-angle-down" />
          </p>
        </div>
      </div>
      <ul className="profile-dropdown onhover-show-div">
        {ProfileListData &&
          ProfileListData.map((item, index) => (
            <li key={index}>
              <Link href={item.path}>
                <FeatherIconCom iconName={item.icon} />
                <span>{item.text} </span>
              </Link>
            </li>
          ))}
        <li onClick={handleLogOut}>
          <a href="#123">
            <FeatherIconCom iconName={"LogIn"} />
            <span>Logout</span>
          </a>
        </li>
      </ul>
    </li>
  );
};

export default Profile;
