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

package de.markusbordihn.easynpc.data.skin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.Constants;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SkinDataEntryTest {

  private static final String SKIN_URL = "https://example.com/skin.png";
  private static final String PLAYER_NAME = "Markus";

  @Test
  @DisplayName("An empty skin only stores its type and its timestamp")
  void testEmptySkinStoresNoEmptyValues() {
    CompoundTag compoundTag = new SkinDataEntry().createTag();

    assertFalse(compoundTag.contains(SkinDataEntry.DATA_NAME_TAG));
    assertFalse(compoundTag.contains(SkinDataEntry.DATA_URL_TAG));
    assertFalse(compoundTag.contains(SkinDataEntry.DATA_UUID_TAG));
    assertFalse(compoundTag.contains(SkinDataEntry.DATA_DISABLE_LAYERS_TAG));
    assertEquals(SkinType.DEFAULT.name(), compoundTag.getStringOr(SkinDataEntry.DATA_TYPE_TAG, ""));
  }

  @Test
  @DisplayName("A skin without a stored UUID reads back as the blank UUID")
  void testMissingUuidFallsBackToBlankUuid() {
    SkinDataEntry skinDataEntry = new SkinDataEntry(new SkinDataEntry().createTag());

    assertEquals(Constants.BLANK_UUID, skinDataEntry.uuid());
    assertEquals("", skinDataEntry.name());
    assertEquals("", skinDataEntry.url());
  }

  @Test
  @DisplayName("A player skin survives a round trip")
  void testPlayerSkinSurvivesRoundTrip() {
    UUID playerUUID = UUID.randomUUID();
    SkinDataEntry skinDataEntry =
        new SkinDataEntry(SkinDataEntry.createPlayerSkin(PLAYER_NAME, playerUUID).createTag());

    assertEquals(PLAYER_NAME, skinDataEntry.name());
    assertEquals(playerUUID, skinDataEntry.uuid());
    assertEquals(SkinType.PLAYER_SKIN, skinDataEntry.type());
  }

  @Test
  @DisplayName("A remote skin survives a round trip")
  void testRemoteSkinSurvivesRoundTrip() {
    SkinDataEntry remoteSkin =
        new SkinDataEntry("", SKIN_URL, UUID.randomUUID(), SkinType.SECURE_REMOTE_URL);

    SkinDataEntry skinDataEntry = new SkinDataEntry(remoteSkin.createTag());

    assertEquals(SKIN_URL, skinDataEntry.url());
    assertEquals(remoteSkin.uuid(), skinDataEntry.uuid());
    assertEquals(SkinType.SECURE_REMOTE_URL, skinDataEntry.type());
  }

  @Test
  @DisplayName("A resource location skin survives a round trip")
  void testResourceLocationSkinSurvivesRoundTrip() {
    Identifier texture = Identifier.fromNamespaceAndPath("example", "textures/entity/npc.png");

    SkinDataEntry skinDataEntry =
        new SkinDataEntry(SkinDataEntry.createResourceLocationSkin(texture).createTag());

    assertEquals(texture, skinDataEntry.texture());
    assertEquals(SkinType.RESOURCE_LOCATION, skinDataEntry.type());
  }

  @Test
  @DisplayName("An invalid texture location is ignored")
  void testInvalidTextureLocationIsIgnored() {
    CompoundTag compoundTag = new SkinDataEntry().createTag();
    compoundTag.putString(SkinDataEntry.DATA_TEXTURE_TAG, "Invalid Texture Location");

    assertNull(new SkinDataEntry(compoundTag).texture());
  }

  @Test
  @DisplayName("Disabled layers are only stored when they are disabled")
  void testDisableLayersIsOnlyStoredWhenSet() {
    assertFalse(new SkinDataEntry().createTag().contains(SkinDataEntry.DATA_DISABLE_LAYERS_TAG));
    assertTrue(
        new SkinDataEntry(new SkinDataEntry().withDisableLayers(true).createTag()).disableLayers());
  }

  @Test
  @DisplayName("A skin of an older version with its removed content is still readable")
  void testLegacyContentTagIsIgnored() {
    CompoundTag legacyTag =
        SkinDataEntry.createPlayerSkin(PLAYER_NAME, UUID.randomUUID()).createTag();
    legacyTag.putString("Content", "legacy-base64-content");

    SkinDataEntry skinDataEntry = new SkinDataEntry(legacyTag);

    assertEquals(PLAYER_NAME, skinDataEntry.name());
    assertEquals(SkinType.PLAYER_SKIN, skinDataEntry.type());
  }
}
