'use client'

import Link from 'next/link'
import { useTranslations } from 'next-intl'
import React from 'react'
import {
  HiOutlineArrowRightOnRectangle,
  HiOutlineGlobeAlt,
} from 'react-icons/hi2'
import { toast } from 'react-toastify'
import Sawl from 'sweetalert2'

import { Avatar } from '@/components/common/Avatar'
import { ProfileListData } from '@/configs/profile'
import { useAppDispatch, useAuth } from '@/hooks'
import { setLoading } from '@/store/slices/themeSlice'

const Profile = () => {
  const dispatch = useAppDispatch()
  const { logout, user } = useAuth()
  const t = useTranslations()

  const logoutConfirm = () => {
    Sawl.fire({
      title: t('confirmation'),
      text: t('logoutConfirm'),
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: t('yes'),
      cancelButtonText: t('no'),
    }).then(({ isConfirmed }) => {
      if (isConfirmed) {
        dispatch(setLoading(true))
        logout()
        toast.success(t('logoutSuccess'))
      }
    })
  }

  return (
    <li className="profile-nav onhover-dropdown pe-0 py-0">
      <div className="media profile-media">
        <Avatar
          image={user?.image ?? null}
          name={user?.name ?? 'User'}
          size={35}
          className="b-r-10"
          rounded
        />
        <div className="media-body">
          <span>{user?.systemName}</span>
          <p className="mb-0 font-roboto">
            {user?.roles?.length
              ? user?.roles?.map((role) => role.name)?.join(', ')
              : t('user')}
            <i className="middle fa fa-angle-down" />
          </p>
        </div>
      </div>
      <ul className="profile-dropdown onhover-show-div">
        <li>
          <a href="/" target="_blank">
            <HiOutlineGlobeAlt />
            <span>{t('website')}</span>
          </a>
        </li>
        {ProfileListData &&
          ProfileListData.map((item, index) => (
            <li key={index}>
              <Link href={item.path}>
                {item.icon}
                <span>{t(item.text)} </span>
              </Link>
            </li>
          ))}
        <li onClick={logoutConfirm}>
          <a href="#123">
            <HiOutlineArrowRightOnRectangle />
            <span>{t('logout')}</span>
          </a>
        </li>
      </ul>
    </li>
  )
}

export default Profile
