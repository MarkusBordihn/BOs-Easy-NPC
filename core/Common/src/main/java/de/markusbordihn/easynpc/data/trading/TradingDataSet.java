/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.trading;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;

public class TradingDataSet {

  public static final StreamCodec<RegistryFriendlyByteBuf, TradingDataSet> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public TradingDataSet decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new TradingDataSet(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, TradingDataSet tradingDataSet) {
          registryFriendlyByteBuf.writeNbt(tradingDataSet.createTag());
        }
      };
  public static final String DATA_TRADING_DATA_SET_TAG = "TradingDataSet";
  public static final String DATA_TRADING_MAX_USES_TAG = "MaxUses";
  public static final String DATA_TRADING_REWARDED_XP_TAG = "RewardedXP";
  public static final String DATA_TRADING_RESETS_EVERY_MIN_TAG = "ResetsEveryMin";
  public static final String DATA_TRADING_LAST_RESET_TAG = "LastReset";
  public static final String DATA_TYPE_TAG = "Type";

  private TradingType tradingType = TradingType.NONE;
  private int maxUses = 64;
  private int rewardedXP = 0;
  private int resetsEveryMin = 0;
  private long lastReset = 0;

  public TradingDataSet() {}

  public TradingDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public TradingType getType() {
    return this.tradingType;
  }

  public void setType(TradingType tradingType) {
    this.tradingType = tradingType;
  }

  public boolean isType(TradingType tradingType) {
    return tradingType != null && this.tradingType == tradingType;
  }

  public int getMaxUses() {
    return this.maxUses;
  }

  public void setMaxUses(int maxUses) {
    this.maxUses = maxUses;
  }

  public int getRewardedXP() {
    return this.rewardedXP;
  }

  public void setRewardedXP(int rewardedXP) {
    this.rewardedXP = rewardedXP;
  }

  public int getResetsEveryMin() {
    return this.resetsEveryMin;
  }

  public void setResetsEveryMin(int resetsEveryMin) {
    this.resetsEveryMin = resetsEveryMin;
  }

  public long getLastReset() {
    return this.lastReset;
  }

  public void setLastReset(long lastReset) {
    this.lastReset = lastReset;
  }

  public void load(CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(DATA_TRADING_DATA_SET_TAG)) {
      return;
    }

    CompoundTag tradingData = compoundTag.getCompound(DATA_TRADING_DATA_SET_TAG);
    this.maxUses = tradingData.getInt(DATA_TRADING_MAX_USES_TAG);
    this.rewardedXP = tradingData.getInt(DATA_TRADING_REWARDED_XP_TAG);
    this.resetsEveryMin = tradingData.getInt(DATA_TRADING_RESETS_EVERY_MIN_TAG);
    this.lastReset =
        tradingData.contains(DATA_TRADING_LAST_RESET_TAG)
            ? tradingData.getLong(DATA_TRADING_LAST_RESET_TAG)
            : System.currentTimeMillis();
    this.tradingType = TradingType.valueOf(tradingData.getString(DATA_TYPE_TAG));
  }

  public CompoundTag save(CompoundTag compoundTag) {
    CompoundTag tradingData = new CompoundTag();
    tradingData.putInt(DATA_TRADING_MAX_USES_TAG, this.maxUses);
    tradingData.putInt(DATA_TRADING_REWARDED_XP_TAG, this.rewardedXP);
    tradingData.putInt(DATA_TRADING_RESETS_EVERY_MIN_TAG, this.resetsEveryMin);
    tradingData.putLong(DATA_TRADING_LAST_RESET_TAG, this.lastReset);
    tradingData.putString(DATA_TYPE_TAG, this.tradingType.name());

    compoundTag.put(DATA_TRADING_DATA_SET_TAG, tradingData);

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }
}
