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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.JsonOps;
import de.markusbordihn.easynpc.Constants;
import java.util.Optional;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.component.DataComponentExactPredicate;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.FloatTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.StringTag;
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
  private static final String COLOR_TAG = "color";
  private static final String BLOCK_ENTITY_TAG = "BlockEntityTag";
  private static final String COMPONENTS_TAG = "components";
  private static final String CONTAINER_COMPONENT = "minecraft:container";
  private static final String CUSTOM_DATA_COMPONENT = Constants.MOD_PREFIX_ID + "custom_data";
  private static final String CUSTOM_MODEL_DATA_TAG = "CustomModelData";
  private static final String CUSTOM_NAME_COMPONENT = "minecraft:custom_name";
  private static final String DISPLAY_TAG = "display";
  private static final String DYED_COLOR_COMPONENT = "minecraft:dyed_color";
  private static final String ENCHANTMENT_GLINT_OVERRIDE_COMPONENT =
      "minecraft:enchantment_glint_override";
  private static final String HIDE_FLAGS_TAG = "HideFlags";
  private static final String ITEM_NAME_COMPONENT = "minecraft:item_name";
  private static final String ITEMS_TAG = "Items";
  private static final String LEGACY_COUNT_TAG = "Count";
  private static final String LEGACY_CUSTOM_DATA_COMPONENT = "minecraft:custom_data";
  private static final String LEGACY_ITEM_TAG = "tag";
  private static final String LOC_NAME_TAG = "LocName";
  private static final String LORE_COMPONENT = "minecraft:lore";
  private static final String LORE_TAG = "Lore";
  private static final String MODERN_COUNT_TAG = "count";
  private static final String NAME_TAG = "Name";
  private static final String RECIPES_TAG = "Recipes";
  private static final String SHOW_IN_TOOLTIP_TAG = "show_in_tooltip";
  private static final String SLOT_TAG = "Slot";
  private static final String UNBREAKABLE_TAG = "Unbreakable";
  private static final String[] TRADE_ITEM_KEYS = {"buy", "buyB", "sell"};

  private TradingUtils() {}

  public static ItemCost getItemCost(ItemStack itemStack) {
    int count = itemStack.getCount() > 0 ? itemStack.getCount() : 1;
    if (itemStack.isEmpty()) {
      return new ItemCost(ItemStack.EMPTY.getItem(), count);
    }
    if (itemStack.getComponentsPatch().isEmpty()) {
      return new ItemCost(itemStack.getItem(), count);
    }
    return new ItemCost(
        itemStack.getItem().builtInRegistryHolder(),
        count,
        DataComponentExactPredicate.allOf(itemStack.getComponents()),
        itemStack.copyWithCount(count));
  }

  public static Optional<ItemCost> getOptionalItemCost(ItemStack itemStack) {
    return itemStack.isEmpty() ? Optional.empty() : Optional.of(getItemCost(itemStack));
  }

  public static Tag migrateLegacyTradeEntry(Tag entry) {
    if (!(entry instanceof CompoundTag compound)) {
      return entry;
    }
    CompoundTag migrated = compound.copy();
    for (String itemKey : TRADE_ITEM_KEYS) {
      if (migrated.getCompound(itemKey).isPresent()) {
        migrated.put(itemKey, migrateLegacyItemData(migrated.getCompoundOrEmpty(itemKey)));
      }
    }
    return migrated;
  }

  private static CompoundTag migrateLegacyItemData(CompoundTag item) {
    CompoundTag migrated = item.copy();

    if (migrated.contains(LEGACY_COUNT_TAG) && !migrated.contains(MODERN_COUNT_TAG)) {
      migrated.putInt(MODERN_COUNT_TAG, migrated.getByteOr(LEGACY_COUNT_TAG, (byte) 1));
      migrated.remove(LEGACY_COUNT_TAG);
    }

    if (migrated.getCompound(LEGACY_ITEM_TAG).isEmpty()) {
      migrateLegacyCustomDataComponent(migrated);
      return migrated;
    }

    CompoundTag legacyTag = migrated.getCompoundOrEmpty(LEGACY_ITEM_TAG).copy();
    CompoundTag components = migrated.getCompoundOrEmpty(COMPONENTS_TAG).copy();

    migrateLegacyTagToComponents(legacyTag, components);
    migrated.remove(LEGACY_ITEM_TAG);

    if (components.isEmpty()) {
      migrated.remove(COMPONENTS_TAG);
    } else {
      migrated.put(COMPONENTS_TAG, components);
    }

    return migrated;
  }

  private static void migrateLegacyTagToComponents(CompoundTag legacyTag, CompoundTag components) {
    int hideFlags = legacyTag.getIntOr(HIDE_FLAGS_TAG, 0);

    moveIntTagToComponent(legacyTag, components, "Damage", "minecraft:damage", 0);
    moveIntTagToComponent(legacyTag, components, "RepairCost", "minecraft:repair_cost", 0);
    moveCustomModelDataToComponent(legacyTag, components);
    moveUnbreakableToComponent(legacyTag, components, hideFlags);
    moveEnchantmentsToComponent(
        legacyTag, components, "Enchantments", "minecraft:enchantments", (hideFlags & 1) != 0);
    moveEnchantmentsToComponent(
        legacyTag,
        components,
        "StoredEnchantments",
        "minecraft:stored_enchantments",
        (hideFlags & 32) != 0);
    moveDisplayTagToComponents(legacyTag, components, hideFlags);
    moveBlockEntityItemsToContainerComponent(legacyTag, components);
    legacyTag.remove(HIDE_FLAGS_TAG);

    if (!legacyTag.isEmpty()) {
      mergeIntoCustomData(components, legacyTag);
    }
  }

  private static void moveIntTagToComponent(
      CompoundTag legacyTag,
      CompoundTag components,
      String tagKey,
      String componentKey,
      int defaultValue) {
    if (legacyTag.getInt(tagKey).isEmpty() || components.contains(componentKey)) {
      return;
    }
    int value = legacyTag.getIntOr(tagKey, defaultValue);
    legacyTag.remove(tagKey);
    if (value != defaultValue) {
      components.putInt(componentKey, value);
    }
  }

  private static void moveCustomModelDataToComponent(
      CompoundTag legacyTag, CompoundTag components) {
    if (legacyTag.getInt(CUSTOM_MODEL_DATA_TAG).isEmpty()
        || components.contains("minecraft:custom_model_data")) {
      return;
    }

    int customModelData = legacyTag.getIntOr(CUSTOM_MODEL_DATA_TAG, 0);
    legacyTag.remove(CUSTOM_MODEL_DATA_TAG);
    CompoundTag customModelDataComponent = new CompoundTag();
    ListTag floats = new ListTag();
    floats.add(FloatTag.valueOf((float) customModelData));
    customModelDataComponent.put("floats", floats);
    components.put("minecraft:custom_model_data", customModelDataComponent);
  }

  private static void moveUnbreakableToComponent(
      CompoundTag legacyTag, CompoundTag components, int hideFlags) {
    if (!legacyTag.getBooleanOr(UNBREAKABLE_TAG, false)
        || components.contains("minecraft:unbreakable")) {
      return;
    }

    legacyTag.remove(UNBREAKABLE_TAG);
    CompoundTag unbreakableComponent = new CompoundTag();
    if ((hideFlags & 4) != 0) {
      unbreakableComponent.putBoolean(SHOW_IN_TOOLTIP_TAG, false);
    }
    components.put("minecraft:unbreakable", unbreakableComponent);
  }

  private static void moveEnchantmentsToComponent(
      CompoundTag legacyTag,
      CompoundTag components,
      String tagKey,
      String componentKey,
      boolean hideFromTooltip) {
    if (legacyTag.getList(tagKey).isEmpty() || components.contains(componentKey)) {
      return;
    }

    ListTag enchantments = legacyTag.getListOrEmpty(tagKey);
    legacyTag.remove(tagKey);
    CompoundTag levels = new CompoundTag();
    for (int i = 0; i < enchantments.size(); i++) {
      CompoundTag enchantment = enchantments.getCompoundOrEmpty(i);
      if (enchantment.getString("id").isEmpty() || enchantment.getInt("lvl").isEmpty()) {
        continue;
      }
      int level = Math.clamp(enchantment.getIntOr("lvl", 0), 0, 255);
      if (level > 0) {
        levels.putInt(enchantment.getStringOr("id", ""), level);
      }
    }

    if (!levels.isEmpty()) {
      CompoundTag enchantmentsComponent = new CompoundTag();
      enchantmentsComponent.put("levels", levels);
      if (hideFromTooltip) {
        enchantmentsComponent.putBoolean(SHOW_IN_TOOLTIP_TAG, false);
      }
      components.put(componentKey, enchantmentsComponent);
    } else if (!enchantments.isEmpty()
        && !components.contains(ENCHANTMENT_GLINT_OVERRIDE_COMPONENT)) {
      components.putBoolean(ENCHANTMENT_GLINT_OVERRIDE_COMPONENT, true);
    }
  }

  private static void moveDisplayTagToComponents(
      CompoundTag legacyTag, CompoundTag components, int hideFlags) {
    if (legacyTag.getCompound(DISPLAY_TAG).isEmpty()) {
      return;
    }

    CompoundTag display = legacyTag.getCompoundOrEmpty(DISPLAY_TAG).copy();

    if (display.getString(NAME_TAG).isPresent() && !components.contains(CUSTOM_NAME_COMPONENT)) {
      components.put(
          CUSTOM_NAME_COMPONENT,
          componentStringToTag(normalizeLegacyComponent(display.getStringOr(NAME_TAG, ""))));
      display.remove(NAME_TAG);
    }

    if (display.getList(LORE_TAG).isPresent() && !components.contains(LORE_COMPONENT)) {
      ListTag normalizedLore = new ListTag();
      ListTag lore = display.getListOrEmpty(LORE_TAG);
      for (int i = 0; i < lore.size(); i++) {
        normalizedLore.add(componentStringToTag(normalizeLegacyComponent(lore.getStringOr(i, ""))));
      }
      if (!normalizedLore.isEmpty()) {
        components.put(LORE_COMPONENT, normalizedLore);
        display.remove(LORE_TAG);
      }
    }

    if (display.getInt(COLOR_TAG).isPresent() && !components.contains(DYED_COLOR_COMPONENT)) {
      CompoundTag dyedColor = new CompoundTag();
      dyedColor.putInt("rgb", display.getIntOr(COLOR_TAG, 0));
      if ((hideFlags & 64) != 0) {
        dyedColor.putBoolean(SHOW_IN_TOOLTIP_TAG, false);
      }
      components.put(DYED_COLOR_COMPONENT, dyedColor);
      display.remove(COLOR_TAG);
    }

    if (display.getString(LOC_NAME_TAG).isPresent() && !components.contains(ITEM_NAME_COMPONENT)) {
      components.put(
          ITEM_NAME_COMPONENT,
          componentStringToTag(createTranslatableComponent(display.getStringOr(LOC_NAME_TAG, ""))));
      display.remove(LOC_NAME_TAG);
    }

    if (display.isEmpty()) {
      legacyTag.remove(DISPLAY_TAG);
    } else {
      legacyTag.put(DISPLAY_TAG, display);
    }
  }

  private static void moveBlockEntityItemsToContainerComponent(
      CompoundTag legacyTag, CompoundTag components) {
    if (legacyTag.getCompound(BLOCK_ENTITY_TAG).isEmpty()) {
      return;
    }

    CompoundTag blockEntityTag = legacyTag.getCompoundOrEmpty(BLOCK_ENTITY_TAG).copy();
    if (blockEntityTag.getList(ITEMS_TAG).isEmpty()) {
      return;
    }

    if (!components.contains(CONTAINER_COMPONENT)) {
      ListTag containerItems = new ListTag();
      ListTag legacyItems = blockEntityTag.getListOrEmpty(ITEMS_TAG);
      for (int i = 0; i < legacyItems.size(); i++) {
        CompoundTag legacyItem = legacyItems.getCompoundOrEmpty(i);
        if (legacyItem.getInt(SLOT_TAG).isEmpty()) {
          continue;
        }

        CompoundTag itemData = legacyItem.copy();
        int slot = itemData.getIntOr(SLOT_TAG, 0);
        if (slot < 0 || slot > 255) {
          continue;
        }
        itemData.remove(SLOT_TAG);
        CompoundTag migratedItem = migrateLegacyItemData(itemData);
        if (migratedItem.getString("id").isEmpty()) {
          continue;
        }

        CompoundTag containerItem = new CompoundTag();
        containerItem.putInt("slot", slot);
        containerItem.put("item", migratedItem);
        containerItems.add(containerItem);
      }

      if (!containerItems.isEmpty()) {
        components.put(CONTAINER_COMPONENT, containerItems);
      }
    }

    blockEntityTag.remove(ITEMS_TAG);
    if (blockEntityTag.isEmpty()) {
      legacyTag.remove(BLOCK_ENTITY_TAG);
    } else {
      legacyTag.put(BLOCK_ENTITY_TAG, blockEntityTag);
    }
  }

  private static void mergeIntoCustomData(CompoundTag components, CompoundTag legacyTag) {
    CompoundTag mergedCustomData = legacyTag.copy();
    mergeCustomDataComponent(components, LEGACY_CUSTOM_DATA_COMPONENT, mergedCustomData);
    mergeCustomDataComponent(components, CUSTOM_DATA_COMPONENT, mergedCustomData);
    components.put(CUSTOM_DATA_COMPONENT, mergedCustomData);
    components.remove(LEGACY_CUSTOM_DATA_COMPONENT);
  }

  private static void migrateLegacyCustomDataComponent(CompoundTag item) {
    if (item.getCompound(COMPONENTS_TAG).isEmpty()) {
      return;
    }

    CompoundTag components = item.getCompoundOrEmpty(COMPONENTS_TAG).copy();
    if (components.getCompound(LEGACY_CUSTOM_DATA_COMPONENT).isEmpty()) {
      return;
    }

    CompoundTag mergedCustomData = new CompoundTag();
    mergeCustomDataComponent(components, LEGACY_CUSTOM_DATA_COMPONENT, mergedCustomData);
    mergeCustomDataComponent(components, CUSTOM_DATA_COMPONENT, mergedCustomData);
    if (!mergedCustomData.isEmpty()) {
      components.put(CUSTOM_DATA_COMPONENT, mergedCustomData);
    }
    components.remove(LEGACY_CUSTOM_DATA_COMPONENT);
    item.put(COMPONENTS_TAG, components);
  }

  private static void mergeCustomDataComponent(
      CompoundTag components, String componentName, CompoundTag target) {
    if (components.getCompound(componentName).isPresent()) {
      target.merge(components.getCompoundOrEmpty(componentName));
    }
  }

  private static String normalizeLegacyComponent(String value) {
    if (value == null || value.isEmpty() || "null".equals(value)) {
      return createTextComponent("");
    }

    char firstCharacter = value.charAt(0);
    char lastCharacter = value.charAt(value.length() - 1);
    if (firstCharacter == '"' && lastCharacter == '"'
        || firstCharacter == '{' && lastCharacter == '}'
        || firstCharacter == '[' && lastCharacter == ']') {
      try {
        JsonElement element = JsonParser.parseString(value);
        if (element.isJsonPrimitive()) {
          return createTextComponent(element.getAsString());
        }
        return element.toString();
      } catch (JsonParseException ignored) {
        // Fallback to a plain text component for malformed legacy values.
      }
    }

    return createTextComponent(value);
  }

  private static Tag componentStringToTag(String jsonComponent) {
    try {
      return JsonOps.INSTANCE.convertTo(NbtOps.INSTANCE, JsonParser.parseString(jsonComponent));
    } catch (RuntimeException ignored) {
      return StringTag.valueOf(jsonComponent);
    }
  }

  private static String createTextComponent(String text) {
    JsonObject textComponent = new JsonObject();
    textComponent.addProperty("text", text);
    return textComponent.toString();
  }

  private static String createTranslatableComponent(String key) {
    JsonObject translatableComponent = new JsonObject();
    translatableComponent.addProperty("translate", key);
    return translatableComponent.toString();
  }

  public static Tag migrateLegacyOffersTag(Tag offersTag) {
    if (!(offersTag instanceof CompoundTag legacyOffers)) {
      return offersTag;
    }
    Tag recipesValue = legacyOffers.get(RECIPES_TAG);
    if (recipesValue instanceof CompoundTag recipesCompound) {
      log.debug("Migrating legacy double-nested trade format (Offers.Recipes.Recipes)");
      return recipesCompound.getListOrEmpty(RECIPES_TAG);
    } else if (recipesValue instanceof ListTag) {
      log.debug("Migrating legacy single-nested trade format (Offers.Recipes)");
      return recipesValue;
    }
    return offersTag;
  }

  static ListTag extractRecipesList(Tag offersTag) {
    if (offersTag instanceof ListTag recipesList) {
      return recipesList;
    }
    if (!(offersTag instanceof CompoundTag offersCompound)) {
      return null;
    }
    Tag recipesTag = offersCompound.get(RECIPES_TAG);
    if (recipesTag instanceof ListTag recipesList) {
      return recipesList;
    }
    if (recipesTag instanceof CompoundTag recipesCompound) {
      Tag nestedRecipesTag = recipesCompound.get(RECIPES_TAG);
      if (nestedRecipesTag instanceof ListTag recipesList) {
        return recipesList;
      }
    }
    return null;
  }

  public static MerchantOffers parseMerchantOffers(
      CompoundTag compoundTag, String offersTag, HolderLookup.Provider provider, String context) {
    Tag rawTag = compoundTag.get(offersTag);
    if (rawTag == null) {
      return null;
    }

    // Legacy format migration: normalize nested/wrapped structures
    Tag migratedTag = migrateLegacyOffersTag(rawTag);
    RegistryOps<Tag> registryOps = provider.createSerializationContext(NbtOps.INSTANCE);

    ListTag legacyRecipesList = extractRecipesList(migratedTag);
    if (legacyRecipesList != null) {
      if (legacyRecipesList.isEmpty()) {
        return null;
      }
      return parseFromList(legacyRecipesList, registryOps, context);
    }

    // Native format: CompoundTag with "Recipes" key use MerchantOffers.CODEC
    if (migratedTag != rawTag) {
      compoundTag.put(offersTag, migratedTag);
    }
    DataResult<MerchantOffers> dataResult =
        MerchantOffers.CODEC.parse(registryOps, compoundTag.get(offersTag));

    Optional<MerchantOffers> parsedOffers = dataResult.result();
    if (parsedOffers.isPresent()) {
      MerchantOffers offers = sanitizeTradingOffers(parsedOffers.get());
      return offers.isEmpty() ? null : offers;
    }

    // Per-entry fallback for native format parse failures
    ListTag recipesList = extractRecipesList(compoundTag.get(offersTag));
    if (recipesList == null) {
      log.error(
          "Failed to parse trade list for {}: {}",
          context,
          dataResult.error().map(e -> e.message()).orElse("unknown error"));
      return null;
    }
    if (recipesList.isEmpty()) {
      return null;
    }

    log.warn(
        "Failed to parse trade list for {}, attempting per-entry recovery: {}",
        context,
        dataResult.error().map(e -> e.message()).orElse("unknown error"));

    return parseFromList(recipesList, registryOps, context);
  }

  private static MerchantOffers parseFromList(
      ListTag recipesList, RegistryOps<Tag> registryOps, String context) {
    MerchantOffers recovered = new MerchantOffers();
    int skipped = 0;
    for (int i = 0; i < recipesList.size(); i++) {
      DataResult<MerchantOffer> entryResult =
          MerchantOffer.CODEC.parse(registryOps, migrateLegacyTradeEntry(recipesList.get(i)));
      Optional<MerchantOffer> parsedEntry = entryResult.result();
      if (parsedEntry.isPresent()) {
        recovered.add(parsedEntry.get());
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
    boolean changed = false;
    for (MerchantOffer offer : offers) {
      if (offer == null || offer.getResult().isEmpty() || offer.getResult().getCount() <= 0) {
        filteredCount++;
        changed = true;
        continue;
      }
      ItemStack costA = offer.getBaseCostA();
      ItemStack costB = offer.getCostB();
      boolean costAValid = !costA.isEmpty() && costA.getCount() > 0;
      boolean costBValid = !costB.isEmpty() && costB.getCount() > 0;
      if (!costAValid && !costBValid) {
        filteredCount++;
        changed = true;
        continue;
      }
      if (!costAValid && costBValid) {
        changed = true;
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
        changed = true;
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
    return changed ? sanitized : offers;
  }
}
