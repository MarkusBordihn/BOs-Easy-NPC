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

package de.markusbordihn.easynpc.security;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.world.entity.Entity;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("PresetSanitizer Tests")
class PresetSanitizerTest {

  private static CompoundTag createPresetData(CommandPermissionLevel commandPermissionLevel) {
    CompoundTag presetData = new CompoundTag();
    presetData.putString(Entity.ID_TAG, "easy_npc:humanoid");

    CompoundTag actionDataTag = new CompoundTag();
    actionDataTag.putInt(
        ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG,
        CommandPermissionLevel.OWNERS.minecraftLevel());

    CompoundTag actionEntryTag = new CompoundTag();
    actionEntryTag.putInt(
        ActionDataEntry.DATA_PERMISSION_LEVEL_TAG, commandPermissionLevel.minecraftLevel());

    ListTag actionEntries = new ListTag();
    actionEntries.add(actionEntryTag);
    actionDataTag.put("Entries", actionEntries);
    presetData.put(ActionEventDataCapable.DATA_ACTION_DATA_TAG, actionDataTag);

    return presetData;
  }

  @Test
  @DisplayName("Should rewrite imported authority and clamp command permissions")
  void testImportSanitization() {
    UUID trustedOwner = UUID.randomUUID();
    CompoundTag presetData = createPresetData(CommandPermissionLevel.OWNERS);
    presetData.putUUID(OwnerDataCapable.DATA_OWNER_TAG, UUID.randomUUID());

    PresetAuthority authority =
        new PresetAuthority(
            trustedOwner,
            CommandPermissionLevel.ALL,
            PresetTrustLevel.UNTRUSTED_PLAYER,
            NpcSecurityRole.NORMAL_PLAYER);
    PresetSanitizationResult result = PresetSanitizer.sanitize(presetData, authority);
    CompoundTag sanitizedTag = result.sanitizedTag();
    CompoundTag actionDataTag =
        sanitizedTag.getCompound(ActionEventDataCapable.DATA_ACTION_DATA_TAG);
    CompoundTag actionEntryTag =
        actionDataTag.getList("Entries", net.minecraft.nbt.Tag.TAG_COMPOUND).getCompound(0);

    assertTrue(result.changed());
    assertEquals(trustedOwner, sanitizedTag.getUUID(OwnerDataCapable.DATA_OWNER_TAG));
    assertEquals(
        CommandPermissionLevel.ALL.minecraftLevel(),
        actionDataTag.getInt(ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG));
    assertEquals(
        CommandPermissionLevel.ALL.minecraftLevel(),
        actionEntryTag.getInt(ActionDataEntry.DATA_PERMISSION_LEVEL_TAG));
    assertTrue(result.notices().contains(PresetSanitizationNotice.OWNER_REWRITTEN));
    assertTrue(result.notices().contains(PresetSanitizationNotice.ACTION_PERMISSION_CLAMPED));
    assertTrue(result.notices().contains(PresetSanitizationNotice.COMMAND_PERMISSION_CLAMPED));
  }

  @Test
  @DisplayName("Should remove authority from exported presets but keep action command requirements")
  void testExportSanitization() {
    CompoundTag presetData = createPresetData(CommandPermissionLevel.ADMINS);
    presetData.putUUID(Entity.UUID_TAG, UUID.randomUUID());
    presetData.putUUID(OwnerDataCapable.DATA_OWNER_TAG, UUID.randomUUID());
    presetData.put("Pos", new ListTag());
    presetData.put("Rotation", new ListTag());

    CompoundTag sanitizedTag = PresetSanitizer.sanitizeForExport(presetData);
    CompoundTag actionDataTag =
        sanitizedTag.getCompound(ActionEventDataCapable.DATA_ACTION_DATA_TAG);
    CompoundTag actionEntryTag =
        actionDataTag.getList("Entries", net.minecraft.nbt.Tag.TAG_COMPOUND).getCompound(0);

    assertFalse(sanitizedTag.contains(Entity.UUID_TAG));
    assertFalse(sanitizedTag.contains(OwnerDataCapable.DATA_OWNER_TAG));
    assertFalse(sanitizedTag.contains("Pos"));
    assertFalse(sanitizedTag.contains("Rotation"));
    assertFalse(actionDataTag.contains(ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG));
    assertEquals(
        CommandPermissionLevel.ADMINS.minecraftLevel(),
        actionEntryTag.getInt(ActionDataEntry.DATA_PERMISSION_LEVEL_TAG));
  }
}
