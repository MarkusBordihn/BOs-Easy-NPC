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

import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.HashMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;

public class TradingDataSet {

  public static final String DATA_TRADING_DATA_SET_TAG = "TradingDataSet";
  public static final String DATA_TRADING_MAX_USES_TAG = "MaxUses";
  public static final String DATA_TRADING_REWARDED_XP_TAG = "RewardedXP";
  public static final String DATA_TRADING_RESETS_EVERY_MIN_TAG = "ResetsEveryMin";
  public static final String DATA_TRADING_LAST_RESET_TAG = "LastReset";
  public static final String DATA_TYPE_TAG = "Type";
  public static final String DATA_OFFER_ACTIONS_TAG = "OfferActions";
  public static final String DATA_OFFER_ACTION_INDEX_TAG = "Index";
  public static final TradingType DEFAULT_TRADING_TYPE = TradingType.NONE;
  public static final int DEFAULT_MAX_USES = 64;
  public static final int DEFAULT_REWARDED_XP = 0;
  public static final int DEFAULT_RESETS_EVERY_MIN = 0;
  public static final long DEFAULT_LAST_RESET = 0;
  public static final StreamCodec<RegistryFriendlyByteBuf, TradingDataSet> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public TradingDataSet decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new TradingDataSet(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, TradingDataSet tradingDataSet) {
          registryFriendlyByteBuf.writeNbt(
              EntityDataSerializersManager.validateAndGetNbt(
                  tradingDataSet.createTag(), "TradingDataSet"));
        }
      };
  private final HashMap<Integer, ActionDataSet> offerActions = new HashMap<>();
  private TradingType tradingType = DEFAULT_TRADING_TYPE;
  private int maxUses = DEFAULT_MAX_USES;
  private int rewardedXP = DEFAULT_REWARDED_XP;
  private int resetsEveryMin = DEFAULT_RESETS_EVERY_MIN;
  private long lastReset = DEFAULT_LAST_RESET;

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

  public boolean hasOfferAction(int offerIndex) {
    ActionDataSet actionDataSet = this.offerActions.get(offerIndex);
    return actionDataSet != null && !actionDataSet.isEmpty();
  }

  public ActionDataSet getOfferAction(int offerIndex) {
    return this.offerActions.getOrDefault(offerIndex, new ActionDataSet());
  }

  public void setOfferAction(int offerIndex, ActionDataSet actionDataSet) {
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      this.offerActions.remove(offerIndex);
    } else {
      this.offerActions.put(offerIndex, actionDataSet);
    }
  }

  public Map<Integer, ActionDataSet> getOfferActions() {
    return this.offerActions;
  }

  public void load(CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(DATA_TRADING_DATA_SET_TAG)) {
      return;
    }

    CompoundTag tradingData = compoundTag.getCompoundOrEmpty(DATA_TRADING_DATA_SET_TAG);
    this.maxUses = tradingData.getInt(DATA_TRADING_MAX_USES_TAG).orElse(DEFAULT_MAX_USES);
    this.rewardedXP = tradingData.getInt(DATA_TRADING_REWARDED_XP_TAG).orElse(DEFAULT_REWARDED_XP);
    this.resetsEveryMin =
        tradingData.getInt(DATA_TRADING_RESETS_EVERY_MIN_TAG).orElse(DEFAULT_RESETS_EVERY_MIN);
    this.lastReset =
        tradingData.contains(DATA_TRADING_LAST_RESET_TAG)
            ? tradingData.getLong(DATA_TRADING_LAST_RESET_TAG).orElse(System.currentTimeMillis())
            : System.currentTimeMillis();
    this.tradingType = TradingType.get(tradingData.getString(DATA_TYPE_TAG).orElse(""));

    this.offerActions.clear();
    if (tradingData.contains(DATA_OFFER_ACTIONS_TAG)) {
      ListTag offerActionsList = tradingData.getListOrEmpty(DATA_OFFER_ACTIONS_TAG);
      for (int i = 0; i < offerActionsList.size(); i++) {
        CompoundTag entryTag = offerActionsList.getCompoundOrEmpty(i);
        int index = entryTag.getInt(DATA_OFFER_ACTION_INDEX_TAG).orElse(0);
        ActionDataSet actionDataSet =
            new ActionDataSet(entryTag, ActionDataSet.ACTION_DATA_SET_TAG);
        if (!actionDataSet.isEmpty()) {
          this.offerActions.put(index, actionDataSet);
        }
      }
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    CompoundTag tradingData = new CompoundTag();
    if (this.maxUses != DEFAULT_MAX_USES) {
      tradingData.putInt(DATA_TRADING_MAX_USES_TAG, this.maxUses);
    }
    if (this.rewardedXP != DEFAULT_REWARDED_XP) {
      tradingData.putInt(DATA_TRADING_REWARDED_XP_TAG, this.rewardedXP);
    }
    if (this.resetsEveryMin != DEFAULT_RESETS_EVERY_MIN) {
      tradingData.putInt(DATA_TRADING_RESETS_EVERY_MIN_TAG, this.resetsEveryMin);
    }
    tradingData.putLong(DATA_TRADING_LAST_RESET_TAG, this.lastReset);
    if (this.tradingType != DEFAULT_TRADING_TYPE) {
      tradingData.putString(DATA_TYPE_TAG, this.tradingType.name());
    }

    if (!this.offerActions.isEmpty()) {
      ListTag offerActionsList = new ListTag();
      for (Map.Entry<Integer, ActionDataSet> entry : this.offerActions.entrySet()) {
        ActionDataSet actionDataSet = entry.getValue();
        if (actionDataSet != null && !actionDataSet.isEmpty()) {
          CompoundTag entryTag = new CompoundTag();
          entryTag.putInt(DATA_OFFER_ACTION_INDEX_TAG, entry.getKey());
          actionDataSet.save(entryTag, ActionDataSet.ACTION_DATA_SET_TAG);
          offerActionsList.add(entryTag);
        }
      }
      if (!offerActionsList.isEmpty()) {
        tradingData.put(DATA_OFFER_ACTIONS_TAG, offerActionsList);
      }
    }

    compoundTag.put(DATA_TRADING_DATA_SET_TAG, tradingData);

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }
}
