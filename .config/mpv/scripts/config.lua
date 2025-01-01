local M = {}

M.keymaps = {
    left  = 'seek -10',
    right = 'seek 10',
    space = 'cycle pause',

    q = 'quit',
    z = 'playlist-next',
    Z = 'playlist-prev',
    x = 'add volume 5',
    X = 'add volume -10',
    m = 'cycle mute',
    s = 'cycle sub down',
    S = 'cycle sub visibility',

    h = 'add video-pan-x  -0.01',
    j = 'add video-pan-y   0.01',
    k = 'add video-pan-y  -0.01',
    l = 'add video-pan-x   0.01',
    f = 'add video-zoom    0.1',
    F = 'add video-zoom   -0.1',
    r = 'set video-zoom    0.01 ; ' ..
        'set video-pan-x   0 ; ' ..
        'set video-pan-y   0 ; ' ..
        'set video-rotate  0',

    ['/, MBTN_RIGHT'] = 'vf toggle hflip',
    e = 'cycle_values video-rotate 90 180 270 0',

    f1 = 'af toggle equalizer=w=1:t=o:f=55:g=16:r=f64,dynaudnorm=b=1:m=1',
    f2 = 'af toggle asetrate=44100*1.08',
    f3 = 'af toggle asetrate=44100*1.12',
    f4 = 'af toggle asetrate=44100*1.22',
    f5 = 'af toggle asetrate=44100*1.32',
    f6 = 'af toggle asetrate=44100*1.52',
}

M.options = {
    volume = '60',
    volume_max = '100',
    ao = 'alsa',
    audio_device = 'alsa/plug:dmix',

    vo = 'gpu',
    video_sync = 'audio',
    audio_display = 'no',

    scale = 'ewa_lanczos',
    cscale = 'ewa_lanczos',
    dscale = 'lanczos',
    dither_depth = 'auto',
    correct_downscaling = 'yes',
    linear_downscaling = 'yes',
    sigmoid_upscaling = 'yes',

    cache = 'yes',
    demuxer_max_bytes = '20M',
    demuxer_max_back_bytes = '20M',

    loop_file = 'yes',
    input_default_bindings = 'no',
    input_builtin_bindings = 'no',

    alpha             = 'no',
    background_color  = '#fcf5ee',
    geometry          = '17%',
    ontop             = 'yes',
    window_dragging   = 'yes',
    video_zoom        = '0.01',

    osc               = 'no',
    osd_level         = '0',
    osd_align_y       = 'top',

    sub_font          = 'Maple Mono',
    sub_color         = '#fcf5ee',
    sub_shadow_offset = '2',
    sub_shadow_color  = '#5b5958',
    sub_border_size   = '0',
    sub_margin_x      = '70',

    slang = 'nl,jp,jpn,en,eng',
    alang = 'nl,jp,jpn,en,eng',
}

return M
