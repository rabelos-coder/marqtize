import Image from "next/image";
import Link from "next/link";
import { useTranslations } from "next-intl";
import React from "react";
import Sawl from "sweetalert2";

import FeatherIconCom from "@/components/common/Icons/FeatherIconCom";
import { ProfileListData } from "@/configs/profile";
import { useAuth } from "@/hooks";

const Profile = () => {
  const { signOut, user } = useAuth();
  const t = useTranslations("translations");

  const signOutConfirm = () => {
    Sawl.fire({
      title: t("confirmation"),
      text: t("logoutConfirm"),
      icon: "question",
      showCancelButton: true,
      confirmButtonText: t("yes"),
      cancelButtonText: t("no"),
    }).then(({ isConfirmed }) => {
      if (isConfirmed) {
        signOut();
      }
    });
  };

  return (
    <li className="profile-nav onhover-dropdown pe-0 py-0">
      <div className="media profile-media">
        <Image
          className="b-r-10"
          src={user?.image ?? "/assets/images/dashboard/profile.png"}
          alt=""
          width={35}
          height={35}
        />
        <div className="media-body">
          <span>{user?.systemName}</span>
          <p className="mb-0 font-roboto">
            {user?.roles?.map((item) => item.name).join(", ") ?? t("user")}{" "}
            <i className="middle fa fa-angle-down" />
          </p>
        </div>
      </div>
      <ul className="profile-dropdown onhover-show-div">
        {ProfileListData &&
          ProfileListData.map((item, index) => (
            <li key={index}>
              <Link href={item.path}>
                <FeatherIconCom iconName={item.icon} />
                <span>{t(item.text)} </span>
              </Link>
            </li>
          ))}
        <li onClick={signOutConfirm}>
          <a href="#123">
            <FeatherIconCom iconName={"LogIn"} />
            <span>{t("logout")}</span>
          </a>
        </li>
      </ul>
    </li>
  );
};

export default Profile;
