# -*- coding: utf-8 -*-
"""
Created on Thu Dec  1 14:58:27 2022

@author: jpacheco
"""


# %% 1) Imports and main paths
import os
import re
import copy
import zipfile

import pandas as pd
from osgeo import gdal, osr
from pyproj import Proj, transform

import numpy as np
import collections
from itertools import combinations_with_replacement

# import pyGNDiv as gn
import matplotlib.pyplot as plt

import importlib.util
# specify the module path that needs to be imported
if os.path.isdir('D://MPI-BGC//1_oBEF-Accross2//RD1_1'):
    spec = importlib.util.spec_from_file_location('pyGNDiv', (
        'D://MPI-BGC//1_oBEF-Accross2//RD1_1//0_Packages//pyGNDiv-numba-master//' +
        'pyGNDiv//pyGNDiv.py'))
elif os.path.isdir('//Net//Groups//BGI//people//jpacheco//1_oBEF-Accross2//RD1_1'):
    spec = importlib.util.spec_from_file_location('pyGNDiv', (
        '//Net//Groups//BGI//people//jpacheco//1_oBEF-Accross2//RD1_1//' +
        '0_Packages//pyGNDiv-numba-master//pyGNDiv//pyGNDiv.py'))
# creates a new module based on spec
gn = importlib.util.module_from_spec(spec)
# executes the module in its own namespace when imported or reloaded.
spec.loader.exec_module(gn)


# %% 2) Functions
def get_S2_NDVI(R, NIR):
    return((NIR - R) / (NIR + R))


def R_from_NDVImax(S2_b, ndvi_, ndvi_max, sz_):
    S2_max_ndvi = np.zeros((sz_[0], sz_[1], sz_[3])) * np.nan
    ii_, jj_ = 0, 0
    for ii_ in range(sz_[0]):
        for jj_ in range(sz_[1]):
            kk_ = np.where(ndvi_[ii_, jj_, :] == ndvi_max[ii_, jj_])[0]
            if len(kk_) > 0:
                if len(kk_) > 1:
                    kk_ = kk_[0]
                S2_max_ndvi[ii_, jj_, :] = S2_b[ii_, jj_, kk_, :]
    return(S2_max_ndvi)

def latlon_to_pixel(lat, lon, input_ds):
    # Get the input image's projection
    input_proj = input_ds.GetProjection()
    input_srs = osr.SpatialReference()
    input_srs.ImportFromWkt(input_proj)

    # Get the input image's geotransform parameters
    geotransform = input_ds.GetGeoTransform()

    # Create a transformation object to convert from lat/lon to image coordinates
    transform = osr.CoordinateTransformation(input_srs, input_srs.CloneGeogCS())

    # Reproject the lat/lon coordinates to the image's coordinate system
    # print(lon, lat)
    point = transform.TransformPoint(lon, lat)

    # Calculate pixel coordinates
    x_pixel = int((point[0] - geotransform[0]) / geotransform[1])
    y_pixel = int((point[1] - geotransform[3]) / geotransform[5])

    return(x_pixel, y_pixel)


def build_coord_matrix(input_ds):
    # Get the input image's geotransform parameters
    geotransform = input_ds.GetGeoTransform()

    # Get image size
    cols = input_ds.RasterXSize
    rows = input_ds.RasterYSize

    # Build x and y coordinates matrices
    x_coords = np.empty((rows, cols))
    y_coords = np.empty((rows, cols))

    for y in range(rows):
        for x in range(cols):
            x_coord = (geotransform[0] + x * geotransform[1] +
                       y * geotransform[2])
            y_coord = (geotransform[3] + x * geotransform[4] +
                       y * geotransform[5])
            x_coords[y, x] = x_coord
            y_coords[y, x] = y_coord

    return(x_coords, y_coords)


def create_raster_from_matrix(matrix, input_ds):
    # Get the input file's geotransform parameters
    geotransform = input_ds.GetGeoTransform()
    
    # Get the input file's projection
    input_proj = input_ds.GetProjection()
    input_srs = osr.SpatialReference()
    input_srs.ImportFromWkt(input_proj)
    
    # Determine matrix dimensions
    rows, cols = matrix.shape

    # Create new raster dataset in memory
    driver = gdal.GetDriverByName('MEM')
    raster_ds = driver.Create('', cols, rows, 1, gdal.GDT_Float32)

    # Set projection and geotransform
    raster_ds.SetProjection(input_proj)
    raster_ds.SetGeoTransform(geotransform)

    # Write matrix data to raster band
    band = raster_ds.GetRasterBand(1)
    band.WriteArray(matrix)

    # Flush to memory
    band.FlushCache()

    return(raster_ds)


def reproject_to_latlon(input_ds, target_ds):
    # Get target file's geotransform parameters
    target_geo = target_ds.GetGeoTransform()

    # Get target file's projection
    target_proj = target_ds.GetProjection()

    # Define target projection
    target_srs = osr.SpatialReference()
    target_srs.ImportFromWkt(target_proj)

    # Get input raster size
    cols = target_ds.RasterXSize
    rows = target_ds.RasterYSize

    # Create output raster in memory
    driver = gdal.GetDriverByName('MEM')
    output_ds = driver.Create('', cols, rows, 1, gdal.GDT_Float32)
    output_ds.SetProjection(target_proj)
    output_ds.SetGeoTransform(target_geo)

    # Reproject the data
    reprojected_data = gdal.ReprojectImage(
        input_ds, output_ds, input_ds.GetProjection(),
        target_proj, gdal.GRA_Bilinear)


    return(output_ds.GetRasterBand(1).ReadAsArray())


def get_distance_filter(b_data, coords_, fname_, sel_radious=300,
                        print_coord=False):
#    try:
    # This does not work in Jupyer
    
    # Get UTM zone
    zone = int((coords_['LONGITUDE'].values + 180.0) / 6) + 1
    # The EPSG code is 32600+zone for positive latitudes and
    # 32700+zone for negatives
    if coords_['LATITUDE'].values >= 0:
        EPSG = 'EPSG:%d' % (32600 + zone)
    else:
        EPSG = 'EPSG:%d' % (32700 + zone)
        
    # Get tower coordinates in UTM
    myProj = Proj(EPSG)
    x_utm, y_utm = myProj(coords_['LONGITUDE'].values[0],
                          coords_['LATITUDE'].values[0])

    # Reproject band virtually
    band_rep = gdal.Warp('', fname_, dstSRS=EPSG, format='VRT')
    Xm_, Ym_ = build_coord_matrix(band_rep)
    
    # Get the tower pixel:
    x_pix, y_pix = latlon_to_pixel(y_utm, x_utm, band_rep)
    
    coords_out = (coords_['LATITUDE'].values[0],
                  coords_['LONGITUDE'].values[0],
                  y_utm, x_utm)

    if print_coord:
        print('Lat/Long: %f, %f; UTM: %f, %f' % (
            coords_out[0], coords_out[1], coords_out[2], coords_out[3]))

    dist_ = np.sqrt((Xm_ - x_utm) ** 2 + (Ym_ - y_utm) ** 2)

    # Get the distance boack to latlon so that the matrix has the same size
    dist_utm = create_raster_from_matrix(dist_, band_rep)
    dist_latlon = reproject_to_latlon(dist_utm, b_data)    

    if ((b_data.RasterYSize != dist_latlon.shape[0]) or
        (b_data.RasterXSize != dist_latlon.shape[1])):
        raise ValueError('The output distance matrix has a different shape than the input image')

    # Filter distances
    dist_filter = dist_latlon > sel_radious

#    except:
#        raise valueError
#        dist_latlon = np.zeros((b_data.RasterYSize, b_data.RasterXSize))
#        dist_filter = np.zeros((b_data.RasterYSize, b_data.RasterXSize),
#                               dtype=bool)

    return(dist_filter, dist_latlon, coords_out)

    
def get_S2_site_cube_data(site_, origen_, coords_, sel_radious=300,
                          run_type=''):
    print(site_)

    b_data = gdal.Open(origen_ + site_ % bands_S2[0])
    sz_ = [b_data.RasterYSize, b_data.RasterXSize, b_data.RasterCount,
           len(bands_S2)]

    doy_data = gdal.Open(origen_ + site_ % 'DOY')
    year_data = gdal.Open(origen_ + site_ % 'YEAR')
    if os.path.exists(origen_ + site_ % 'SCL') and (run_type != '_incl_baresoil'):
        scl_data = gdal.Open(origen_ + site_ % 'SCL')
        scl_available = True
    else:
        scl_available = False

    # Preallocate the 4D cube and load data
    S2_b = np.zeros(sz_) * np.nan
    # j_, i_ = 0, 0
    for i_ in range(sz_[2]):
        # print(i_)
        b_data = gdal.Open(origen_ + site_ % bands_S2[0])
        dist_filter, dist_, coords_out = get_distance_filter(
            b_data, coords_, origen_ + site_ % bands_S2[0],
            sel_radious=sel_radious, print_coord=(i_==0))
        
        if False and (i_ == 0):            
            plt.clf()
            plt.imshow(dist_)
            plt.colorbar()
            plt.title('%s\nLat/Long: %f, %f; UTM: %f, %f' % (
                site_.split('_')[0], coords_out[0], coords_out[1],
                coords_out[2], coords_out[3]), fontsize=9)
            plt.savefig(((origen_ + site_ % bands_S2[0] +
                          '_dist.png').replace('.tif', '').replace(
                'S2_L2A_fluxnet_tmp' + run_type + 'S2_fluxsites_cutouts',
                'S2_L2A_fluxnet' + run_type)))
            plt.close()
            
            plt.clf()
            plt.imshow(dist_filter)
            plt.colorbar()
            plt.title('%s\nLat/Long: %f, %f; UTM: %f, %f' % (
                site_.split('_')[0], coords_out[0], coords_out[1],
                coords_out[2], coords_out[3]), fontsize=9)
            plt.savefig(((origen_ + site_ % bands_S2[0] +
                          '_dist_fil.png').replace('.tif', '').replace(
                'S2_L2A_fluxnet_tmp' + run_type + '//S2_fluxsites_cutouts',
                'S2_L2A_fluxnet' + run_type)))
            plt.close()

        if scl_available:
            SCL = scl_data.GetRasterBand(i_+1).ReadAsArray() != 4
        else:
            SCL = np.zeros(sz_[:2], dtype=bool)

        for j_ in range(sz_[3]):
            b_data = gdal.Open(origen_ + site_ % bands_S2[j_])
            # print(bands_S2[j_])
            tmp_bnd = b_data.GetRasterBand(i_+1).ReadAsArray() / 10000.
            # print([tmp_bnd.shape, SCL.shape, dist_filter.shape])
            tmp_bnd[(tmp_bnd == 0.) | SCL | dist_filter] = np.nan
            S2_b[:, :, i_, j_] = tmp_bnd

    return(S2_b, doy_data, year_data, sz_)


# %% Functions for pyGNDiv package
def cube2TAmatrix(im_cube, wsize_=3, get_max_dis_=True):
    # Get size. Must be (y, x, spectral)
    sz_ = im_cube.shape
    sz_xy = sz_[0] * sz_[1]

    # Convert to 2D matrix
    cube2D = np.reshape(im_cube, (sz_xy, sz_[2]))

    # Find finite data
    Ifinite_ = np.isfinite(np.sum(cube2D, axis=1))

    # Apply PCA
    (pca_comps, max_dist_Eucl, max_dist_SS,
     explained_variance_ratio_) = gn.apply_std_PCA(cube2D[Ifinite_, :],
                                                   get_max_dis_=get_max_dis_)
    cube_PC_2D = np.zeros((sz_xy, pca_comps.shape[1])) * np.nan
    cube_PC_2D[Ifinite_, :] = pca_comps

    # Moving window index
    im_indx = np.zeros((sz_[0], sz_[1]), dtype=int)
    iw_, jw_, kw_ = 0, 0, 0
    for iw_ in range(0, sz_[0], wsize_):
        iw2_ = min(iw_ + wsize_, sz_[0])
        for jw_ in range(0, sz_[1], wsize_):
            jw2_ = min(jw_ + wsize_, sz_[1])
            im_indx[iw_:iw2_, jw_:jw2_] = kw_
            kw_ += 1
    im_indx_2D = np.reshape(im_indx, sz_xy)
    num_windows = copy.deepcopy(kw_)

    # Preallocate output image index
    out_im_indx = im_indx[0:sz_[0]:wsize_, 0:sz_[1]:wsize_]
    out_indx = out_im_indx.reshape(-1)

    # Generate traits and abundances matrices only with the useable data
    Isp = np.zeros(im_indx_2D.shape, dtype=bool)
    Icm = np.zeros(num_windows, dtype=bool)
    abundances_ = np.zeros((num_windows, sz_xy))
    numel_window = wsize_**2
    kw_ = 0
    for kw_ in range(num_windows):
        Iw = np.where(im_indx_2D == kw_)[0]
        if np.sum(Ifinite_[Iw]) == numel_window:
            Icm[kw_] = True
            Isp[Iw] = True
            abundances_[kw_, Iw] = 1.

    # Compute relative abundances
    abundances_ = abundances_[Icm, :] / numel_window
    abundances_ = abundances_[:, Isp]
    # np.sum(abundances_, axis=1)
    traits_PCA = cube_PC_2D[Isp, :]
    comm_index = out_indx[Icm]
    return(traits_PCA, abundances_, comm_index, out_im_indx, Isp,
           max_dist_Eucl, max_dist_SS, explained_variance_ratio_)


def image_RaoQ(X, freq_, alphas_=np.ones(1), normalize_dist=False, max_dist_=0.,
               wsize_=3, nan_tolerance=0.):
    # comb = np.array(list(combinations_with_replacement(
    #     range(X.shape[0]), 2)), dtype=np.uint32)
    # I_diag = gn.get_Idiag(comb)

    RaoQ = np.zeros(freq_.shape[0]) * np.nan
    for is_ in range(freq_.shape[0]):
        I_ = freq_[is_] > 0.
        # RaoQ[is_], _ = gn.mpaRaoS_freq_i(
        #     X[I_, :], comb, freq_[is_, I_].reshape(1, -1), I_diag,
        #     alphas_, nan_tolerance=nan_tolerance,
        #     normalize_dist=normalize_dist, max_dist_=max_dist_)
        RaoQ[is_], _ = gn.mpaRaoS_freq(
            X[I_, :], freq_[is_, I_].reshape(1, -1),
            alphas_=alphas_, nan_tolerance=nan_tolerance,
            normalize_dist=normalize_dist, max_dist_=max_dist_)
    return(RaoQ)


# Maybe not for the package
def processCube(im_cube, bi_red, bi_nir, wsize_=3, get_max_dis_=True,
                get_indices=False):
    sz_ = im_cube.shape
    tmp_bnds_2D = np.reshape(im_cube, (sz_[0] * sz_[1], sz_[2]))
    if np.any(np.isfinite(im_cube)):
        # Process spectral cube
        (traits_PCA, abundances_, comm_index, out_im_indx, Isp,
         max_dist_Eucl, max_dist_SS, explained_variance_ratio_) = (
            cube2TAmatrix(im_cube, wsize_=wsize_, get_max_dis_=get_max_dis_))
        # Get RaoQ
        RaoQ = image_RaoQ(traits_PCA, abundances_, normalize_dist=get_max_dis_,
                          max_dist_=max_dist_Eucl, wsize_=wsize_)
        f_valid = len(RaoQ) / out_im_indx.size

        # Get NDVI and NIRv
        if get_indices:
            ndvi_ = get_S2_NDVI(tmp_bnds_2D[Isp, bi_red],
                                tmp_bnds_2D[Isp, bi_nir]).reshape(-1, 1)
            nirv_ = ndvi_ * tmp_bnds_2D[Isp, bi_nir].reshape(-1, 1)

            RaoQ_ndvi = image_RaoQ(ndvi_, abundances_,
                                   normalize_dist=get_max_dis_,
                                   max_dist_=max_Eucl_dist_1D, wsize_=3)
            RaoQ_nirv = image_RaoQ(nirv_, abundances_,
                                   normalize_dist=get_max_dis_,
                                   max_dist_=max_Eucl_dist_1D, wsize_=3)
        else:
            ndvi_, nirv_, RaoQ_ndvi, RaoQ_nirv = np.nan, np.nan, np.nan, np.nan
        # plt.clf()
        # plt.plot(RaoQ, RaoQ_ndvi, 'or')
        # plt.plot(RaoQ, RaoQ_nirv, 'og')
        # plt.plot([0, .016], RaoQ_nirv, 'og')
        # plt.clf()
        # plt.hist(RaoQ.reshape(-1))
        # plt.hist(RaoQ_ndvi.reshape(-1))
        # plt.hist(RaoQ_nirv.reshape(-1))
        return(RaoQ, tmp_bnds_2D, Isp, f_valid, RaoQ_ndvi, RaoQ_nirv,
               ndvi_, nirv_)
    else:
        Isp = np.zeros((sz_[0] * sz_[1]), dtype=bool)
        return(np.nan, tmp_bnds_2D, Isp, 0., np.nan, np.nan, np.nan, np.nan)


def plot_data_available(X, title_str, fname):
    V =  np.isfinite(np.sum(X, axis=3))
    plt.clf()
    plt.imshow(np.sum(V, axis=2), vmin=0, vmax=V.shape[0])
    plt.colorbar()
    plt.title(title_str)
    plt.savefig(fname)
    plt.close()


# %% Sentinel L2A bands
if True:
    print('Starting S2 L2A...')
    ori_ = os.getcwd()
    if ori_[:16] == '/Net/Groups/BGI/':
        path_S2 = '//Net//Groups//BGI//'
    else:
        path_S2 = 'M://'
    run_type = '_incl_baresoil'
    # run_type = '_no_baresoil'
    origen_zip = path_S2 + 'work_2//ugomar//S2_L2A_fluxnet' + run_type + '//'
    dest_zip = path_S2 + 'scratch//jpacheco//S2_Ulisse//S2_L2A_fluxnet_tmp' + run_type + '//'
    dest_ =  path_S2 + 'scratch//jpacheco//S2_Ulisse//S2_L2A_fluxnet' + run_type + '//'
    os.makedirs(dest_, exist_ok=True)
    preallocate = False

    # Unzip files in temporary scracth folder
    if os.path.isdir(dest_zip) is False:
        print('Unzipping...')
        os.makedirs(dest_zip, exist_ok=True)
        list_zip = sorted([d_ for d_ in os.listdir(origen_zip) if
                    re.findall('S2_fluxsites_cutouts-', d_)])
        # zp_ = list_zip[0]
        for zp_ in [list_zip[-1]]:
            with zipfile.ZipFile(origen_zip + zp_, 'r') as zip_ref:
                zip_ref.extractall(dest_zip)

    # Get sites
    origen_ = dest_zip + 'S2_fluxsites_cutouts//'
    list_sites = [d_ for d_ in os.listdir(origen_) if
                  re.findall('bandB2', d_)]
    db_coords = pd.read_csv(origen_zip + 'coords.csv')

    # Band selection from Ma et al., 2019
    bands_S2 = ['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B11', 'B12']
    bi_red = bands_S2.index('B4')
    bi_nir = bands_S2.index('B8')
    max_Eucl_dist_1D = gn.max_eucl_dist_PCA_fun(1, fexp_var=1., n_sigmas=6.)


    cols_i = (['Year', 'DoY', 'f_valid_samples',
               'RaoQ_S2_median', 'RaoQ_S2_std',
               'RaoQ_NDVI_S2_median', 'RaoQ_NDVI_S2_std',
               'RaoQ_NIRv_S2_median', 'RaoQ_NIRv_S2_std',
               'NDVI_median', 'NDVI_std', 'NIRv_median', 'NIRv_std'] +
              [b_ + '_median' for b_ in bands_S2] +
              [b_ + '_std' for b_ in bands_S2] +
              [b_ + '_median_fdm' for b_ in bands_S2] +
              [b_ + '_std_fdm' for b_ in bands_S2])
    dfi_dummy = pd.DataFrame(data=np.zeros((1, len(cols_i))
                                           ) * np.nan, columns=cols_i)

    # s_ = 0
    for s_ in range(len(list_sites)):
        site_ = list_sites[s_].replace('bandB2', 'band%s')
        site_id = site_.split('_')[0]
        site_out_fname = dest_ + (site_.replace('.tif', '.csv')).replace(
            'band%s', 'bands').replace('1C_bands', '2A_bands')
        coords_ = db_coords[db_coords['SITE_ID'] == site_id]

        if os.path.exists(site_out_fname) is False:
            # Preallocate and save empty file to run several jobs
            if preallocate:
                dfi_dummy.to_csv(site_out_fname, sep=';')

            # try:
            # Get data cube of the site
            (S2_b, doy_data, year_data, sz_) = get_S2_site_cube_data(
                site_, origen_, coords_, sel_radious=300, run_type=run_type)
            plot_data_available(
                S2_b, (site_.replace('_', '-')).replace('-band%s', ''),
                site_out_fname.replace('.csv', '.png'))

            # Preallocate
            dfi_ = pd.DataFrame(
                data=np.zeros((sz_[2], len(cols_i))) * np.nan,
                columns=cols_i)

            # Compute diversity metrics per date
            # i_ = 9
            for i_ in range(sz_[2]):
                try:
                    # im_cube = copy.deepcopy(S2_b[:, :, i_, :])
                    (RaoQ, tmp_bnds_2D, Isp, f_valid, RaoQ_ndvi, RaoQ_nirv,
                    ndvi, nirv) = processCube(S2_b[:, :, i_, :], bi_red,
                                                bi_nir, get_indices=True)

                    # Assign outputs
                    dfi_.loc[i_, 'Year'] = np.nanmean(
                        year_data.GetRasterBand(i_+1).ReadAsArray())
                    dfi_.loc[i_, 'DoY'] = np.nanmean(
                        doy_data.GetRasterBand(i_+1).ReadAsArray())
                    dfi_.loc[i_, 'f_valid_samples'] = f_valid

                    if f_valid > 0:
                        dfi_.loc[i_, 'RaoQ_S2_median'] = np.nanmedian(RaoQ)
                        dfi_.loc[i_, 'RaoQ_S2_std'] = np.nanstd(RaoQ)                        
                        dfi_.loc[i_, 'RaoQ_NDVI_S2_median'] = (
                            np.nanmedian(RaoQ_ndvi))
                        dfi_.loc[i_, 'RaoQ_NDVI_S2_std'] = (
                            np.nanstd(RaoQ_ndvi))
                        dfi_.loc[i_, 'RaoQ_NIRv_S2_median'] = (
                            np.nanmedian(RaoQ_nirv))
                        dfi_.loc[i_, 'RaoQ_NIRv_S2_std'] = (
                            np.nanstd(RaoQ_nirv))

                        dfi_.loc[i_, 'NDVI_S2_median'] = np.nanmedian(ndvi)
                        dfi_.loc[i_, 'NDVI_S2_std'] = np.nanstd(ndvi)
                        dfi_.loc[i_, 'NIRv_S2_median'] = np.nanmedian(nirv)
                        dfi_.loc[i_, 'NIRv_S2_std'] = np.nanstd(nirv)

                        for b_ in range(len(bands_S2)):
                            dfi_.loc[i_, bands_S2[b_] + '_median'] = (
                                np.nanmedian(tmp_bnds_2D[:, b_]))
                            dfi_.loc[i_, bands_S2[b_] + '_std'] = (
                                np.nanstd(tmp_bnds_2D[:, b_]))

                        if np.any(Isp):
                            for b_ in range(len(bands_S2)):
                                dfi_.loc[i_, bands_S2[b_] + '_median_fdm'] = (
                                    np.nanmedian(tmp_bnds_2D[Isp, b_]))
                                dfi_.loc[i_, bands_S2[b_] + '_std_fdm'] = (
                                    np.nanstd(tmp_bnds_2D[Isp, b_]))
                except:
                    print('An error occurred in %s at date %d' %
                          (site_, i_))

            # Store daily data if necessary
            dfi_.to_csv(site_out_fname, sep=';')


