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

package de.markusbordihn.easynpc.data.skin;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;

public record SkinDataEntry(
    String name,
    String url,
    UUID uuid,
    SkinType type,
    boolean disableLayers,
    String content,
    long timestamp) {

  static final String DATA_NAME_TAG = "Name";
  static final String DATA_TYPE_TAG = "Type";
  static final String DATA_URL_TAG = "URL";
  static final String DATA_UUID_TAG = "UUID";
  static final String DATA_DISABLE_LAYERS_TAG = "DisableLayers";
  static final String DATA_CONTENT_TAG = "Content";
  static final String DATA_TIMESTAMP_TAG = "Timestamp";
  public static final StreamCodec<RegistryFriendlyByteBuf, SkinDataEntry> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public SkinDataEntry decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new SkinDataEntry(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, SkinDataEntry skinDataEntry) {
          registryFriendlyByteBuf.writeNbt(skinDataEntry.createTag());
        }
      };

  public SkinDataEntry() {
    this("", "", Constants.BLANK_UUID, SkinType.DEFAULT, false, "", System.currentTimeMillis());
  }

  public SkinDataEntry(final String name, final String url, final UUID uuid, final SkinType type) {
    this(name, url, uuid, type, false, "", System.currentTimeMillis());
  }

  public SkinDataEntry(final CompoundTag compoundTag) {
    this(
        compoundTag.contains(DATA_NAME_TAG) ? compoundTag.getString(DATA_NAME_TAG) : "",
        compoundTag.contains(DATA_URL_TAG) ? compoundTag.getString(DATA_URL_TAG) : "",
        compoundTag.contains(DATA_UUID_TAG)
            ? compoundTag.getUUID(DATA_UUID_TAG)
            : Constants.BLANK_UUID,
        SkinType.get(compoundTag.getString(DATA_TYPE_TAG)),
        compoundTag.contains(DATA_DISABLE_LAYERS_TAG)
            && compoundTag.getBoolean(DATA_DISABLE_LAYERS_TAG),
        compoundTag.contains(DATA_CONTENT_TAG) ? compoundTag.getString(DATA_CONTENT_TAG) : "",
        compoundTag.contains(DATA_TIMESTAMP_TAG)
            ? compoundTag.getLong(DATA_TIMESTAMP_TAG)
            : System.currentTimeMillis());
  }

  public static SkinDataEntry createNoneSkin() {
    return new SkinDataEntry("", "", Constants.BLANK_UUID, SkinType.NONE);
  }

  public static SkinDataEntry createDefaultSkin(String variantName) {
    return new SkinDataEntry(variantName, "", Constants.BLANK_UUID, SkinType.DEFAULT);
  }

  public static SkinDataEntry createCustomSkin(UUID skinUUID, boolean disableLayers) {
    return new SkinDataEntry(
        "", "", skinUUID, SkinType.CUSTOM, disableLayers, "", System.currentTimeMillis());
  }

  public static SkinDataEntry createPlayerSkin(String playerName, UUID playerUUID) {
    return new SkinDataEntry(playerName, "", playerUUID, SkinType.PLAYER_SKIN);
  }

  public static SkinDataEntry createRemoteSkin(String skinURL) {
    return new SkinDataEntry(
        "",
        skinURL,
        UUID.nameUUIDFromBytes(skinURL.getBytes()),
        UrlValidator.isSecureRemoteUrl(skinURL)
            ? SkinType.SECURE_REMOTE_URL
            : SkinType.INSECURE_REMOTE_URL);
  }

  public SkinDataEntry withName(final String name) {
    return new SkinDataEntry(
        name, this.url, this.uuid, this.type, this.disableLayers, this.content, this.timestamp);
  }

  public SkinDataEntry withType(final SkinType type) {
    return new SkinDataEntry(
        this.name, this.url, this.uuid, type, this.disableLayers, this.content, this.timestamp);
  }

  public SkinDataEntry withURL(final String url) {
    return new SkinDataEntry(
        this.name, url, this.uuid, this.type, this.disableLayers, this.content, this.timestamp);
  }

  public SkinDataEntry withUUID(final UUID uuid) {
    return new SkinDataEntry(
        this.name, this.url, uuid, this.type, this.disableLayers, this.content, this.timestamp);
  }

  public SkinDataEntry withDisableLayers(final boolean disableLayers) {
    return new SkinDataEntry(
        this.name, this.url, this.uuid, this.type, disableLayers, this.content, this.timestamp);
  }

  public SkinDataEntry create(CompoundTag compoundTag) {
    return new SkinDataEntry(compoundTag);
  }

  public CompoundTag write(CompoundTag compoundTag) {
    compoundTag.putString(DATA_NAME_TAG, this.name);
    compoundTag.putString(DATA_TYPE_TAG, this.type.name());
    compoundTag.putString(DATA_URL_TAG, this.url);
    compoundTag.putUUID(DATA_UUID_TAG, this.uuid);
    compoundTag.putBoolean(DATA_DISABLE_LAYERS_TAG, this.disableLayers);
    compoundTag.putString(DATA_CONTENT_TAG, this.content);
    compoundTag.putLong(DATA_TIMESTAMP_TAG, this.timestamp);
    return compoundTag;
  }

  public CompoundTag createTag() {
    return write(new CompoundTag());
  }
}
