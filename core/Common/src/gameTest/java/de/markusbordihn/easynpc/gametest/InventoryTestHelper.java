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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.attribute.InventoryAttributeType;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetDataUtils;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.Container;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.phys.Vec3;

public class InventoryTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(1, 2, 2);
  private static final int EMERALD_COUNT = 7;

  private InventoryTestHelper() {}

  private static Container getInventory(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCInventoryData().getEasyNPCInventory();
  }

  private static ItemEntity spawnEmeraldsAt(GameTestHelper helper, Mob mob) {
    ItemEntity itemEntity =
        new ItemEntity(
            helper.getLevel(),
            mob.getX(),
            mob.getY(),
            mob.getZ(),
            new ItemStack(Items.EMERALD, EMERALD_COUNT));
    itemEntity.setDeltaMovement(Vec3.ZERO);
    helper.getLevel().addFreshEntity(itemEntity);
    return itemEntity;
  }

  private static int countDroppedEmeralds(GameTestHelper helper, Mob mob) {
    return helper
        .getLevel()
        .getEntitiesOfClass(ItemEntity.class, mob.getBoundingBox().inflate(3.0))
        .stream()
        .map(ItemEntity::getItem)
        .filter(itemStack -> itemStack.is(Items.EMERALD))
        .mapToInt(ItemStack::getCount)
        .sum();
  }

  public static void assertItemsArePickedUpIntoInventory(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    AttributeHandler.setInventoryAttribute(easyNPC, InventoryAttributeType.PICK_UP_ITEMS, true);
    ItemEntity itemEntity = spawnEmeraldsAt(helper, mob);

    mob.aiStep();

    GameTestHelpers.assertEquals(
        helper,
        "Picked up items land in the NPC inventory",
        EMERALD_COUNT,
        getInventory(easyNPC).countItem(Items.EMERALD));
    GameTestHelpers.assertTrue(helper, "The picked up item entity is gone", itemEntity.isRemoved());
    GameTestHelpers.assertTrue(
        helper, "Picked up items are not equipped", mob.getMainHandItem().isEmpty());
  }

  public static void assertNothingIsPickedUpByDefault(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    spawnEmeraldsAt(helper, mob);

    mob.aiStep();

    GameTestHelpers.assertTrue(
        helper, "A new NPC leaves its inventory empty", getInventory(easyNPC).isEmpty());
  }

  public static void assertFullInventoryRejectsItems(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setInventoryAttribute(easyNPC, InventoryAttributeType.PICK_UP_ITEMS, true);
    Container inventory = getInventory(easyNPC);
    for (int slot = 0; slot < inventory.getContainerSize(); slot++) {
      inventory.setItem(slot, new ItemStack(Items.STONE, 64));
    }

    GameTestHelpers.assertTrue(
        helper,
        "A full inventory rejects further items",
        !easyNPC.getMob().wantsToPickUp(helper.getLevel(), new ItemStack(Items.EMERALD)));
  }

  public static void assertInventoryIsDroppedOnDeathWhenEnabled(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    AttributeHandler.setInventoryAttribute(easyNPC, InventoryAttributeType.DROP_ON_DEATH, true);
    getInventory(easyNPC).setItem(0, new ItemStack(Items.EMERALD, EMERALD_COUNT));

    mob.kill(helper.getLevel());

    GameTestHelpers.assertEquals(
        helper,
        "The inventory is dropped on death",
        EMERALD_COUNT,
        countDroppedEmeralds(helper, mob));
  }

  public static void assertInventoryIsNotDroppedOnDeathByDefault(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    getInventory(easyNPC).setItem(0, new ItemStack(Items.EMERALD, EMERALD_COUNT));

    mob.kill(helper.getLevel());

    GameTestHelpers.assertEquals(
        helper, "The inventory is not dropped by default", 0, countDroppedEmeralds(helper, mob));
  }

  public static void assertInventoryIsExportedWithThePreset(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setInventoryAttribute(easyNPC, InventoryAttributeType.PICK_UP_ITEMS, true);
    AttributeHandler.setInventoryAttribute(easyNPC, InventoryAttributeType.DROP_ON_DEATH, true);
    getInventory(easyNPC).setItem(4, new ItemStack(Items.EMERALD, EMERALD_COUNT));

    CompoundTag presetTag = easyNPC.getEasyNPCPresetData().serializePresetData();
    EasyNPC<?> importedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    importedNPC.getEasyNPCPresetData().importPresetData(presetTag);

    GameTestHelpers.assertEquals(
        helper,
        "The inventory contents are part of an exported preset",
        EMERALD_COUNT,
        getInventory(importedNPC).countItem(Items.EMERALD));
    GameTestHelpers.assertEquals(
        helper,
        "The inventory settings are part of an exported preset",
        easyNPC.getEasyNPCInventoryData().getInventoryAttributes(),
        importedNPC.getEasyNPCInventoryData().getInventoryAttributes());
  }

  public static void assertPresetItemDropsInventoryContents(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setInventoryAttribute(easyNPC, InventoryAttributeType.DROP_ON_DEATH, true);
    getInventory(easyNPC).setItem(0, new ItemStack(Items.EMERALD, EMERALD_COUNT));
    CompoundTag presetTag = easyNPC.getEasyNPCPresetData().serializePresetData();

    PresetData presetItemData =
        PresetDataUtils.fromItemStack(
            PresetDataUtils.toItemStack(new PresetData(entityType, presetTag)));

    GameTestHelpers.assertTrue(
        helper,
        "A preset item carries no inventory contents",
        !presetItemData.data().contains(InventoryDataCapable.DATA_INVENTORY_TAG));
  }
}
