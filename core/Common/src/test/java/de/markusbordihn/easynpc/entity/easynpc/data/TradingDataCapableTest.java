/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.entity.easynpc.data;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import java.util.EnumMap;
import java.util.Optional;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.resources.RegistryOps;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.trading.ItemCost;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TradingDataCapableTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static RegistryAccess.Frozen registries() {
    return RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
  }

  private static RegistryOps<Tag> registryOps() {
    return registries().createSerializationContext(NbtOps.INSTANCE);
  }

  private static MerchantOffer validOffer() {
    return new MerchantOffer(
        new ItemCost(Items.EMERALD),
        Optional.empty(),
        new ItemStack(Items.DIAMOND),
        0,
        7,
        2,
        0.05F);
  }

  @Test
  @DisplayName("negative reset interval never resets offers")
  void testNegativeResetIntervalDoesNotReset() {
    TestTradingData tradingData = new TestTradingData();
    MerchantOffer offer = validOffer();
    offer.increaseUses();
    tradingData.setTradingOffers(new MerchantOffers());
    tradingData.getTradingOffers().add(offer);
    tradingData.getTradingDataSet().setResetsEveryMin(-1);
    tradingData.getTradingDataSet().setLastReset(0L);

    assertFalse(tradingData.resetExpiredTradingOffers());
    assertEquals(1, offer.getUses());
  }

  @Test
  @DisplayName("empty and malformed offers are filtered before merchant offers are exposed")
  void testInvalidOffersAreFiltered() {
    TestTradingData tradingData = new TestTradingData();
    tradingData.getTradingDataSet().setType(TradingType.BASIC);
    MerchantOffers offers = new MerchantOffers();
    offers.add(
        new MerchantOffer(
            new ItemCost(Items.EMERALD), Optional.empty(), ItemStack.EMPTY, 0, 7, 0, 0.05F));
    offers.add(validOffer());
    tradingData.setSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, offers);

    assertDoesNotThrow(tradingData::updateMerchantTradingOffers);

    MerchantOffers merchantOffers = tradingData.getMerchantTradingOffers();
    assertEquals(1, merchantOffers.size());
    assertEquals(Items.DIAMOND, merchantOffers.get(0).getResult().getItem());
  }

  @Test
  @DisplayName("trade uses survive synced offer NBT round-trip")
  void testTradeUsesSurviveOfferRoundTrip() {
    MerchantOffer offer = validOffer();
    offer.increaseUses();
    offer.increaseUses();
    MerchantOffers offers = new MerchantOffers();
    offers.add(offer);

    RegistryOps<Tag> ops = registryOps();
    Tag encoded = MerchantOffers.CODEC.encodeStart(ops, offers).getOrThrow();
    MerchantOffers loaded = MerchantOffers.CODEC.parse(ops, encoded).getOrThrow();

    assertEquals(1, loaded.size());
    assertEquals(2, loaded.get(0).getUses());
    assertEquals(7, loaded.get(0).getMaxUses());
  }

  @Test
  @DisplayName("trading data and used offers are both written during additional save")
  void testAdditionalTradingSaveIncludesDataAndUsedOffers() {
    TestTradingData tradingData = new TestTradingData();
    TradingDataSet dataSet = tradingData.getTradingDataSet();
    dataSet.setType(TradingType.BASIC);
    dataSet.setMaxUses(7);
    dataSet.setResetsEveryMin(30);
    dataSet.setLastReset(123L);
    MerchantOffer offer = validOffer();
    offer.increaseUses();
    MerchantOffers offers = new MerchantOffers();
    offers.add(offer);
    tradingData.setSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, offers);
    CompoundTag savedTag = new CompoundTag();

    tradingData.addAdditionalTradingData(savedTag, registries());

    assertTrue(savedTag.contains(TradingDataCapable.DATA_TRADING_DATA_TAG));
    assertTrue(savedTag.contains(TradingDataCapable.DATA_OFFERS_TAG));
    MerchantOffers loadedOffers =
        MerchantOffers.CODEC
            .parse(registryOps(), savedTag.get(TradingDataCapable.DATA_OFFERS_TAG))
            .getOrThrow();
    assertEquals(1, loadedOffers.get(0).getUses());
  }

  private static final class TestTradingData implements EasyNPC<Mob>, TradingDataCapable<Mob> {

    private final EnumMap<SynchedDataIndex, Object> synchedData =
        new EnumMap<>(SynchedDataIndex.class);
    private MerchantOffers merchantTradingOffers;
    private Player tradingPlayer;
    private int npcDataVersion;

    private TestTradingData() {
      this.defineSynchedTradingData(null);
    }

    @Override
    public Player getTradingPlayer() {
      return this.tradingPlayer;
    }

    @Override
    public void setTradingPlayer(Player player) {
      this.tradingPlayer = player;
    }

    @Override
    public MerchantOffers getMerchantTradingOffers() {
      return this.merchantTradingOffers;
    }

    @Override
    public void setMerchantTradingOffers(MerchantOffers merchantOffers) {
      this.merchantTradingOffers = merchantOffers;
    }

    @Override
    public void rewardTradeXp(MerchantOffer merchantOffer) {}

    @Override
    public void stopTrading() {}

    @Override
    public boolean isClientSide() {
      return false;
    }

    @Override
    public int getNPCDataVersion() {
      return this.npcDataVersion;
    }

    @Override
    public void setNPCDataVersion(int version) {
      this.npcDataVersion = version;
    }

    @Override
    public FakePlayer getFakePlayer(ServerLevel level, BlockPos blockPos) {
      return null;
    }

    @Override
    public <T> void defineSynchedEntityData(
        SynchedEntityData.Builder builder, SynchedDataIndex synchedDataIndex, T defaultData) {
      this.synchedData.putIfAbsent(synchedDataIndex, defaultData);
    }

    @Override
    public <T> void setSynchedEntityData(
        SynchedDataIndex synchedDataIndex, T data, boolean forceUpdate) {
      this.synchedData.put(synchedDataIndex, data);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getSynchedEntityData(SynchedDataIndex synchedDataIndex) {
      return (T) this.synchedData.get(synchedDataIndex);
    }

    @Override
    public GoalSelector getEntityGoalSelector() {
      return null;
    }

    @Override
    public GoalSelector getEntityTargetSelector() {
      return null;
    }
  }
}
