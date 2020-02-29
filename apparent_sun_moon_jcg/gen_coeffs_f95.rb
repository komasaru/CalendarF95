#! /usr/local/bin/ruby
#---------------------------------------------------------------------------------
#= 配列作成
#  * 全ての年の天測暦テキストデータを読み込み、係数ファイルを生成する（以下の8個）
#    + SUN(R.A.)
#    + SUN(Dec.)
#    + SUN(Dist.)
#    + MOON(R.A.)
#    + MOON(Dec.)
#    + MOON(H.P.)
#    + R
#    + EPS
#
# DATE          AUTHOR          VERSION
# 2018.11.07    mk-mode.com     1.00 新規作成
# 2020.02.29    mk-mode.com     1.01 月日の半角数字に対応
#
# Copyright(C) 2018-2020 mk-mode.com All Rights Reserved.
#---------------------------------------------------------------------------------
# 引数 : なし
#---------------------------------------------------------------------------------
#++
require 'kconv'

class GenCoeffsF95
  TXTS = [
    [2008, "text/na08-data.txt"],
    [2009, "text/na09-data.txt"],
    [2010, "text/na10-data.txt"],
    [2011, "text/na11-data.txt"],
    [2012, "text/na12-data.txt"],
    [2013, "text/na13-data.txt"],
    [2014, "text/na14-data.txt"],
    [2015, "text/na15-data.txt"],
    [2016, "text/na16-data.txt"],
    [2017, "text/na17-data.txt"],
    [2018, "text/na18-data.txt"],
    [2019, "text/na19-data.txt"],
    [2020, "text/na20-data.txt"]
  ]
  F_SUN_RA   = "SUN_RA.txt"
  F_SUN_DEC  = "SUN_DEC.txt"
  F_SUN_DIST = "SUN_DIST.txt"
  F_MOON_RA  = "MOON_RA.txt"
  F_MOON_DEC = "MOON_DEC.txt"
  F_MOON_HP  = "MOON_HP.txt"
  F_R        = "R.txt"
  F_EPS      = "EPS.txt"

  def initialize
    @sun_ra   = Array.new
    @sun_dec  = Array.new
    @sun_dist = Array.new
    @moon_ra  = Array.new
    @moon_dec = Array.new
    @moon_hp  = Array.new
    @r        = Array.new
    @eps      = Array.new
  end

  def exec
    gen_array
    output
  rescue => e
    $stderr.puts "[#{e.class}] #{e.message}"
    e.backtrace.each { |tr| $stderr.puts "\t#{tr}"}
    exit 1
  end

  private

  def gen_array
    kbn = ""
    ary = Array.new(9).map { |a| Array.new }
    term = Array.new(3).map { |a| Array.new }

    begin
      TXTS.each do |year, txt|
        File.open(txt, "r:cp932") do |f|
          f.each_line do |l|
            line = l.toutf8.chomp
            line.sub!(/^\s+/, "")
            case line
            when /太陽の/
              kbn = "S"
              ary = Array.new(9).map { |a| Array.new }
              next
            when /Ｒ，黄道傾角/
              kbn = "R"
              ary = Array.new(9).map { |a| Array.new }
              next
            when /月の/
              kbn = "M"
              ary = Array.new(9).map { |a| Array.new }
              next
            when /^$/
              case kbn
              when "S"
                0.upto(2) do |i|
                  @sun_ra   << [year, term[i], ary[i * 3    ]]
                  @sun_dec  << [year, term[i], ary[i * 3 + 1]]
                  @sun_dist << [year, term[i], ary[i * 3 + 2]]
                end
              when "R"
                0.upto(2) do |i|
                  @r   << [year, term[i], ary[i * 2    ]]
                  @eps << [year, term[i], ary[i * 2 + 1]]
                end
              when "M"
                0.upto(2) do |i|
                  @moon_ra  << [year, term[i], ary[i * 3    ]]
                  @moon_dec << [year, term[i], ary[i * 3 + 1]]
                  @moon_hp  << [year, term[i], ary[i * 3 + 2]]
                end
              end
              kbn = ""
              next
            end
            next if kbn == ""
            if /a\s*=\s*(\d+)\s*,\s*b\s*=\s*(\d+).*a\s*=\s*(\d+)\s*,\s*b\s*=\s*(\d+).*a\s*=\s*(\d+)\s*,\s*b\s*=\s*(\d+)/ =~ line
              term = [[$1.to_i, $2.to_i], [$3.to_i, $4.to_i], [$5.to_i, $6.to_i]]
              next
            end
            next if line =~ /月.*?日/
            cols = line.split(/\s+/)
            next unless cols[0] =~ /\d+/
            1.upto(9) { |i| ary[i - 1] << cols[i].to_f }
          end
        end
      end
    rescue => e
      raise
    end
  end

  def output
    begin
      File.open(F_SUN_RA   , "w") { |f| f.puts gen_str("SUN_RA",   @sun_ra  ) }
      File.open(F_SUN_DEC  , "w") { |f| f.puts gen_str("SUN_DEC",  @sun_dec ) }
      File.open(F_SUN_DIST , "w") { |f| f.puts gen_str("SUN_DIST", @sun_dist) }
      File.open(F_MOON_RA  , "w") { |f| f.puts gen_str("MOON_RA",  @moon_ra ) }
      File.open(F_MOON_DEC , "w") { |f| f.puts gen_str("MOON_DEC", @moon_dec) }
      File.open(F_MOON_HP  , "w") { |f| f.puts gen_str("MOON_HP",  @moon_hp ) }
      File.open(F_R        , "w") { |f| f.puts gen_str("R",        @r       ) }
      File.open(F_EPS      , "w") { |f| f.puts gen_str("EPS",      @eps     ) }
    rescue => e
      raise
    end
  end

  def gen_str(name, src)
    str = ""
    src.each_with_index do |row, i|
      str << "%4d %3d %3d " % [row[0], row[1][0], row[1][1]]
      str << row[2].map do |col|
        name =~ /DEC$|EPS/ ? "%9.5f" % col : "%10.6f" % col
      end.join(" ")
      str << "\n"
    end
    return str
  rescue => e
    raise
  end
end

GenCoeffsF95.new.exec if __FILE__ == $0

