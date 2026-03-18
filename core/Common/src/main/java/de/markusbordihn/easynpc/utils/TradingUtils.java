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

package de.markusbordihn.easynpc.utils;

import com.mojang.serialization.DataResult;
import java.util.Optional;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.RegistryOps;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.ItemCost;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TradingUtils {

  private static final Logger log = LogManager.getLogger(TradingUtils.class);

  private TradingUtils() {}

  public static ItemCost getItemCost(ItemStack itemStack) {
    return new ItemCost(
        itemStack.isEmpty() ? ItemStack.EMPTY.getItem() : itemStack.getItem(),
        itemStack.getCount() > 0 ? itemStack.getCount() : 1);
  }

  public static Optional<ItemCost> getOptionalItemCost(ItemStack itemStack) {
    return itemStack.isEmpty() ? Optional.empty() : Optional.of(getItemCost(itemStack));
  }

  public static Tag migrateLegacyTradeEntry(Tag entry) {
    if (!(entry instanceof CompoundTag compound)) {
      return entry;
    }
    CompoundTag migrated = compound.copy();
    for (String itemKey : new String[] {"buy", "buyB", "sell"}) {
      if (migrated.contains(itemKey, Tag.TAG_COMPOUND)) {
        CompoundTag item = migrated.getCompound(itemKey).copy();
        if (item.contains("Count") && !item.contains("count")) {
          item.putInt("count", item.getByte("Count"));
          item.remove("Count");
          item.remove("tag");
          migrated.put(itemKey, item);
        }
      }
    }
    return migrated;
  }

  public static Tag migrateLegacyOffersTag(Tag offersTag) {
    if (!(offersTag instanceof CompoundTag legacyOffers)) {
      return offersTag;
    }
    Tag recipesValue = legacyOffers.get("Recipes");
    if (recipesValue instanceof CompoundTag recipesCompound) {
      log.debug("Migrating legacy double-nested trade format (Offers.Recipes.Recipes)");
      return recipesCompound.getList("Recipes", Tag.TAG_COMPOUND);
    } else if (recipesValue instanceof ListTag) {
      log.debug("Migrating legacy single-nested trade format (Offers.Recipes)");
      return recipesValue;
    }
    return offersTag;
  }

  public static MerchantOffers parseMerchantOffers(
      CompoundTag compoundTag, String offersTag, HolderLookup.Provider provider, String context) {
    Tag rawTag = compoundTag.get(offersTag);
    if (rawTag == null) {
      return null;
    }

    // Legacy format migration
    Tag migratedTag = migrateLegacyOffersTag(rawTag);
    if (migratedTag != rawTag) {
      compoundTag.put(offersTag, migratedTag);
    }

    // Attempt bulk parse
    RegistryOps<Tag> registryOps = provider.createSerializationContext(NbtOps.INSTANCE);
    DataResult<MerchantOffers> dataResult =
        MerchantOffers.CODEC.parse(registryOps, compoundTag.get(offersTag));

    // Bulk parse succeeded
    Optional<MerchantOffers> result = dataResult.result();
    if (result.isPresent()) {
      MerchantOffers offers = sanitizeTradingOffers(result.get());
      return offers.isEmpty() ? null : offers;
    }

    // Per-entry fallback
    Tag offersNbt = compoundTag.get(offersTag);
    if (!(offersNbt instanceof ListTag recipesList)) {
      log.error(
          "Failed to parse trade list for {}: {}",
          context,
          dataResult.error().map(e -> e.message()).orElse("unknown error"));
      return null;
    }
    if (recipesList.isEmpty()) {
      return null;
    }

    log.error(
        "Failed to parse full trade list for {}, attempting per-entry recovery: {}",
        context,
        dataResult.error().map(e -> e.message()).orElse("unknown error"));

    // Attempt to parse entries individually to salvage valid trades
    MerchantOffers recovered = new MerchantOffers();
    int skipped = 0;
    for (int i = 0; i < recipesList.size(); i++) {
      Tag entry = migrateLegacyTradeEntry(recipesList.get(i));
      DataResult<MerchantOffer> entryResult = MerchantOffer.CODEC.parse(registryOps, entry);
      if (entryResult.result().isPresent()) {
        recovered.add(entryResult.result().get());
      } else {
        skipped++;
        log.warn(
            "Skipping malformed trade entry [{}] for {}: {}",
            i,
            context,
            entryResult.error().map(e -> e.message()).orElse("unknown"));
      }
    }
    if (skipped > 0) {
      log.warn(
          "Recovered {}/{} trade(s) for {}, skipped {} malformed entr{}",
          recovered.size(),
          recipesList.size(),
          context,
          skipped,
          skipped == 1 ? "y" : "ies");
    }
    if (recovered.isEmpty()) {
      return null;
    }
    MerchantOffers sanitized = sanitizeTradingOffers(recovered);
    return sanitized.isEmpty() ? null : sanitized;
  }

  public static MerchantOffers sanitizeTradingOffers(MerchantOffers offers) {
    if (offers == null || offers.isEmpty()) {
      return offers;
    }
    MerchantOffers sanitized = new MerchantOffers();
    int filteredCount = 0;
    for (MerchantOffer offer : offers) {
      if (offer == null || offer.getResult().isEmpty() || offer.getResult().getCount() <= 0) {
        filteredCount++;
        continue;
      }
      ItemStack costA = offer.getBaseCostA();
      ItemStack costB = offer.getCostB();
      boolean costAValid = !costA.isEmpty() && costA.getCount() > 0;
      boolean costBValid = !costB.isEmpty() && costB.getCount() > 0;
      if (!costAValid && !costBValid) {
        filteredCount++;
        continue;
      }
      if (!costAValid && costBValid) {
        sanitized.add(
            new MerchantOffer(
                getItemCost(costB),
                Optional.empty(),
                offer.getResult(),
                offer.getUses(),
                offer.getMaxUses(),
                offer.getXp(),
                offer.getPriceMultiplier(),
                offer.getDemand()));
      } else if (costAValid && costBValid) {
        sanitized.add(offer);
      } else if (costAValid) {
        sanitized.add(
            new MerchantOffer(
                getItemCost(costA),
                Optional.empty(),
                offer.getResult(),
                offer.getUses(),
                offer.getMaxUses(),
                offer.getXp(),
                offer.getPriceMultiplier(),
                offer.getDemand()));
      }
    }
    if (filteredCount > 0) {
      log.warn("Sanitized {} invalid trade(s) to prevent crash", filteredCount);
    }
    return sanitized;
  }
}
